(ns scheep.pattern-lang
  (:gen-class)
  (:use
   [clojure.core.unify :only [unify]]
   [clojure.walk :only [prewalk-replace postwalk]] 
   [scheep.env :only [lookup]]))

;;; Implementation for scheme's pattern language used in
;;; syntax-rules

;;; TODO
;;;   1) match dots

(declare literal?
         first-symbol)
;;; Expanding ellipsis

(defn map-form
  "Like core.walk/prewalk, but only applies f to symbols"
  [f form]
  (postwalk #(if (symbol? %) (f %) %) form))
                    
(defn rename
  "Append suffix to each symbol in form"
  [form suffix]
  (map-form #(symbol (str % "-#" suffix)) form))
  
(defn ?+
  "Turns symbols into pattern variables if they
   are not macro literals"
  [pattern literals]
  (defn ?add [s]
    (if-not (literal? literals s)
      (symbol (str "?" s))
      s))
  (map-form ?add pattern))
     
(defn repeat-form
  "Returns a list with form repeated n times, each
   with a unique suffix appended to each symbol"
  [form n]
  (let [forms (repeat n form)
        new-names (range n)]
    (map rename forms new-names)))

(defn expanded-count
  [pattern n]
  (if (list? pattern)
    (into {} (map vector
                  (flatten pattern) 
                  (repeat n)))
    {pattern n}))
  
(defn expand-pattern-1 [pattern form]
  (let [vec-pattern (vec pattern)]
    (if (= (peek vec-pattern) '...)
      (let [rest-1 (pop vec-pattern)
            p  (peek rest-1)
            rest (pop rest-1)
            n (- (count form) (count rest))]
        {:pattern (concat rest (repeat-form p n))
         :n (expanded-count p n)})
      {:pattern pattern
       :n {}})))

(defn expand-rule-1 [rule count-map]
  (if-not (list? rule)
    rule
    (let [vec-rule (vec rule)]
      (if (= (peek vec-rule) '...)
        (let [rest-1 (pop vec-rule)
              p  (peek rest-1)
              rest (pop rest-1)
              n (count-map (first-symbol p))]
          (apply list (concat rest (repeat-form p n))))
        rule))))

(defn expand-rule
  "Replace ellipsis in a rewrite rule by the
   the number of forms given in count-map"
  [rule count-map]
  (postwalk
   #(if (list? %) (expand-rule-1 % count-map) %)
   rule))

(defn expand-pattern
  "Replace ellipsis in a pattern so that the pattern's
   tree structure matches the given form's. The number
   that each symbol is repeated is counted in :n"
  [pattern form]
  (let [{new-pattern :pattern
         counts :n} (expand-pattern-1 pattern form)]
    (loop [[p & ps] new-pattern
           [f & fs] form
           count-map counts
           acc []]
      (cond
       (nil? p) [(apply list acc) count-map]

       (and (list? p) (list? f))
       (let [[acc- count-map-] (expand-pattern p f)]
         (recur ps
                fs
                (merge count-map count-map-)
                (conj acc acc-)))

       :else
       (recur ps fs count-map (conj acc p))))))

(defn expand-ellipsis
  "Replace the ellipsis in a given pattern and rule
   according to the tree structure of form."
  [pattern rule form]
  (let [[new-pattern count-map] (expand-pattern pattern form)
        new-rule (expand-rule rule count-map)]
    [new-pattern new-rule]))

(defn match
  "Returns a substitution map and rule for a given pattern, rule
   and form"
  [form [pattern rule] literals]
  (let [[new-pattern new-rule] (expand-ellipsis pattern rule form)
        ?new-pattern (?+ new-pattern literals)
        rule-smap (zipmap
              (distinct (flatten new-pattern))
              (distinct (flatten ?new-pattern)))
        pattern-smap (unify ?new-pattern form)]
    (if pattern-smap
      (list pattern-smap
            (prewalk-replace rule-smap new-rule)))))

;;; Helpers

(defn literal? [literals p]
  (let [res (not (nil? (some #{p} literals)))]
    res))
  
(defn first-symbol [p]
  (if (symbol? p)
    p
    (first-symbol (first p))))
