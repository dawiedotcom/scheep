(ns scheep.pattern-lang
  (:gen-class)
  (:use
   [scheep.env :only [lookup]]
   [clojure.core.unify :only [unify subst]]))

;;; Implementation for scheme's pattern language used in
;;; syntax-rules

;;; TODO
;;;   1) pass a map instead of four params
;;;   2) get util fns to test for literals and merge maps
;;;   3) match elipsis
;;;   4) match dots

(declare literal?
         same-literal?
         next-pattern
         merge-concat)

(defmulti pattern
  (fn [{[_ pattern2] :pattern}] 
    pattern2))

(defmulti concrete-pattern
  (fn [{[pattern1] :pattern}]
    (class pattern1)))

(defn symbol-pattern
  [{[f & fs] :form
    [p & ps] :pattern
    subs :acc
    literals :literals
    def-env :dev-env
    use-env :use-env
    :as map}]
  (if-not (literal? literals p)
    (pattern (next-pattern map fs ps (assoc subs p (list f))))
    (if (same-literal? def-env p use-env f)
      (pattern (next-pattern map fs ps subs)))))
      
;;; Defaults

;;; Expanding ellipsis

(defmulti map-form (fn [func form] (class form)))
(defmethod map-form :default [func form] form)
(defmethod map-form clojure.lang.Symbol [func form]
  (func form))

(defmethod map-form java.util.Collection [func form]
  ;"Applies func recursively to each symbol in form"
  ;[func form] 
  (loop [[f & fs] form
         acc []]
    (if (nil? f)
      (apply list acc)
      (recur fs (conj acc (map-form func f))))))
     ;(symbol? f) (recur fs (conj acc (map-form func f)))
     ;(list? f) (recur fs (conj acc (map-form func f)))
     ;:else (recur fs (conj acc f)))))
         
(defn rename
  "Append suffix to each symbol in form"
  [form suffix]
  (map-form #(symbol (str % "-#" suffix)) form))
  
(defn ?+ [pattern]
  (map-form #(symbol (str "?" %)) pattern))
     
;(defmulti expand (fn [p n] (class p)))
;
;(defmethod expand clojure.lang.Symbol [p n]
;  (map #(symbol (str p "#" %)) (range n)))
;  ;(rename (repeat n p)
;
;(defmethod expand java.util.Collection [p n]

(defn expand [form n]
  (let [forms (repeat n form)
        new-names (range n)]
    (map rename forms new-names)))

(defn expanded-count [pattern n]
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
        {:pattern (concat rest (expand p n))
         :n (expanded-count p n)})
      {:pattern pattern
       :n {}})))

(defn first-symbol [p]
  (if (symbol? p)
    p
    (first-symbol (first p))))

(defn expand-rule-1 [rule count-map]
  (let [vec-rule (vec rule)]
    (if (= (peek vec-rule) '...)
      (let [rest-1 (pop vec-rule)
            p  (peek rest-1)
            rest (pop rest-1)
            n (count-map (first-symbol p))]
        (concat rest (expand p n)))
      rule)))

(defn expand-rule [rule count-map]
  ;(defn map-helper [elem]
  ;  (if (sequential? elem)
  ;    (expand-rule-1 elem count-map)
  ;    elem))
  ;(map map-helper rule))
  (loop [[r & rs] (expand-rule-1 rule count-map)
         acc []]
    (cond
     (nil? r) (apply list acc)
     (list? r) (recur rs (conj acc (expand-rule r count-map)))
     :else (recur rs (conj acc r)))))

(defn expand-pattern [pattern form]
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

(defn expand-ellipsis [pattern rule form]
  (let [[new-pattern count-map] (expand-pattern pattern form)
        new-rule (expand-rule rule count-map)]
    [new-pattern new-rule]))

(defn get-pattern
  [form [pattern rule]]
  (println "get-pattern: \n" pattern "\n" rule "\n" form)
  (let [[new-pattern new-rule] (expand-ellipsis pattern rule form)]
    (list (unify (?+ new-pattern) form) new-rule)))

(defn sub [pattern rule form]
  (let [[new-pattern new-rule] (expand-ellipsis pattern rule form)
        sub-map (unify (?+ new-pattern) form)]
    ;(println sub-map)
    (if sub-map
      (subst (?+ new-rule) sub-map))))
    ;(let [transcribed (subst (?+ new-rule) sub-map)]
    ;  transcribed)))
        

;(defmethod pattern :default [args] (concrete-pattern args))
(defmethod pattern :default [{form :form pattern :pattern :as args}] )
(defmethod concrete-pattern :default [args] nil)

;;; Pattern

(defmethod pattern
  '...
  [{form :form [p] :pattern subs :acc :as arg-map}]
  "Match the rest of the forms against the first pattern"
  (if-not (or (list? form) (nil? form))
    (throw (ex-info "Ill formed special form:" {:cause (:exp arg-map)})))
  (if (empty? form)
    (merge-concat subs
                  (if (list? p)
                    ; if p is a list and form is empty, all the 
                    ; patterns in p should expand to nothing.
                    (apply merge
                           (cons {p nil}
                                 (map (fn [i] {i nil}) p)))
                    ; p is an identifier, should expand to nothing.
                    {p nil '... nil}))
    ;; all the forms in forms should be matched by p
    (apply merge-concat
           (cons subs 
                 (map #(pattern
                        (next-pattern arg-map (list %) (list p) {'... nil}))
                      form)))))

;;; Concrete pattern

(defmethod concrete-pattern
  nil
  [{form :form subs :acc :as map}]
  "The terminal case"
  (if (empty? form) subs))

(defmethod concrete-pattern
  clojure.lang.Symbol
  [map]
  "first pattern is a symbol, but not ... or ."
  (symbol-pattern map))

(defmethod concrete-pattern
  java.util.Collection
  [{form :form [p & ps] :pattern subs :acc :as map}]
  "first pattern is a list, match recursively into the tree"
  (if-not (list? form)
    (throw (ex-info "Ill formed special form:" {:cause form})))
  (let [[f & fs] form
        recur-map (next-pattern map f p {})
        merged (merge-concat subs (pattern recur-map))]
    (if merged
      (pattern (next-pattern map fs ps merged)))))

;;; Helpers

(defn next-pattern [map form pattern acc]
  "Returns a new argememt map with replaced vals for
   form, pattern and acc"
  (assoc map :form form :pattern pattern :acc acc))

(defn literal? [literals p]
  (let [res (not (nil? (some #{p} literals)))]
    res))
  
(defn same-literal? [s-env-def p s-env-use f]
  (let [res (= (lookup p s-env-def)
               (lookup f s-env-use))]
    res))

(defn merge-concat [& maps]
  ;; assuming each map has only lists as values, this
  ;; does the same as merge, but concats vals that
  ;; correspond to the same key.
  (defn reducer [m1 m2]
    ;(println "\n" m1 "\n" m2 "\n")
    (if (and m1 m2)
      (let [ks (concat (keys m1) (keys m2))
            vs (map #(concat (m1 %) (m2 %)) ks)]
        (zipmap ks vs))))
  (reduce reducer maps))

