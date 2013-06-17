(ns scheep.pattern-lang
  (:gen-class)
  (:use
   [clojure.core.unify :only [unify]]
   [clojure.walk :only [prewalk-replace postwalk]] 
   [scheep.primitives :only [pair?]]
   [scheep.env :only [lookup]]))

;;; Implementation for scheme's pattern language used in
;;; syntax-rules

(declare literal?
         first-symbol
         repeat-form
         expanded-count
         make-unify-vars)

;;; Expanding ellipsis

(defn expand-with
  ([f sexpr]
   (expand-with f sexpr sexpr))
  ([f sexpr default] 
   (if (and (sequential? sexpr) (> (count sexpr) 1))
     (let [vec-rule (vec sexpr)
           n (peek vec-rule)
           rest-1 (pop vec-rule)
           n-1  (peek rest-1)
           rest (pop rest-1)]
       (f rest n-1 n))
     default)))

(defn expand-pattern-1 [pattern form]
  (let [unexpanded {:pattern pattern :n {}}]
    (expand-with
     (fn [rest p last]
       (if (= last '...)
         (let [n (- (count form) (count rest))]
           {:pattern (doall (concat rest (repeat-form p n)))
            :n (expanded-count p n)})
         unexpanded))
     pattern
     unexpanded)))

(defn expand-rule-1 [rule count-map]
  (expand-with
   (fn [rest p last]
     (if (= last '...)
       (let [n (count-map (first-symbol p))]
         (apply list (concat rest (repeat-form p n))))
       rule))
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
           seen-dot? false
           acc []]
      (cond

       (and (nil? p)
            seen-dot?) [(apply list acc) count-map]

       (and (nil? f)
            (nil? p)) [(apply list acc) count-map]

       (nil? p) [nil count-map]

       (and (pair? p) (list? f))
       (let [p-pair (list (.car p) '. (.cdr p))
             [acc- count-map-] (expand-pattern p-pair f)]
         (recur ps
                fs
                (merge count-map count-map-)
                (or seen-dot? (= p '.))
                (conj acc acc-)))

       (and (list? p) (list? f))
       (let [[acc- count-map-] (expand-pattern p f)]
         (recur ps
                fs
                (merge count-map count-map-)
                (or seen-dot? (= p '.))
                (conj acc acc-)))

       :else
       (recur ps fs count-map (or seen-dot? (= p '.)) (conj acc p))))))

(defn expand-rule
  "Replace ellipsis in a rewrite rule by the
   the number of forms given in count-map"
  [rule count-map]
  (postwalk
   #(cond
     (pair? %) (list '. (.car %) (.cdr %))
     (list? %) (expand-rule-1 % count-map)
     :else %)
   rule))

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
        ?new-pattern (make-unify-vars new-pattern literals)
        pattern-symbols (distinct (flatten new-pattern))
        pattern-vars (distinct (flatten ?new-pattern))
        rule-smap (into {}
                        (remove (fn [[k v]] (or (= k v) (= [k v] ['. '&])))
                                (zipmap pattern-symbols pattern-vars)))
        pattern-smap (unify ?new-pattern form)]
    (if (and pattern-smap
             (= (set (keys pattern-smap))
                (set (vals rule-smap))))
      ; Succeed if we have a map that replaces all the variables in
      ; the pattern
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

(defn map-form
  "Like core.walk/prewalk, but only applies f to symbols"
  [f form]
  (postwalk #(if (symbol? %) (f %) %) form))
                    
(defn make-unify-vars
  "Turns symbols into pattern variables if they
   are not macro literals"
  [pattern literals]
  (defn ?add [s]
    (cond
     (= s '.) '&
     (not (literal? literals s)) (symbol (str "?" s))
     :else s))
    #_(if-not (literal? literals s)
      (symbol (str "?" s))
      s)
  (map-form ?add pattern))
     
(defn rename
  "Append suffix to each symbol in form"
  [form suffix]
  (map-form #(symbol (str % "-#" suffix)) form))
  
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
