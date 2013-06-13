(ns scheep.pattern-lang
  (:gen-class)
  (:use
   [scheep.env :only [lookup]]))

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

(defmethod pattern :default [args] (concrete-pattern args))
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

