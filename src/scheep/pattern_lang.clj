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
  (fn [{[_ pattern2] :pattern :as map}] 
    ;(println "\n== pattern\n map = \n" map)
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
  ;(println "\nform " form "\n")
  (if (empty? form)
    (merge-concat subs {p nil '... nil})
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
  [{[f & fs] :form [p & ps] :pattern subs :acc :as map}]
  "first pattern is a list, match recursively into the tree"
  (let [recur-map (next-pattern map f p {})
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
    ;(println "literal?: " res " literals: " literals " p: " p)
    res))
  
(defn same-literal? [s-env-def p s-env-use f]
  (let [res (= (lookup p s-env-def)
               (lookup f s-env-use))]
    ;(println "same-literal?: " res)
    res))

(defn merge-concat [& maps]
  ;; assuming each map has only lists as values, this
  ;; does the same as merge, but concats vals that
  ;; correspond to the same key.
  (defn reducer [m1 m2]
    ;(println "\n" m1 "\n" m2 "\n")
    (if (and m1 m2)
      (let [ks (concat (keys m1) (keys m2))
            vs (map #(concat (% m1) (% m2)) ks)]
        (zipmap ks vs))))
  (reduce reducer maps))

(comment
(defn match [[macro-name & exp-args]
             [[_ & pattern-vars] rewrite-rule]
             literals
             s-env-use
             s-env-def]
  (defn literal? [p]
    (some #{p} literals))
  (defn same-literal? [p f]
    (= (lookup p s-env-def)
       (lookup f s-env-use)))
    
  (defn iter [[f & fs :as forms] [p & ps] subs]
    (cond
      ; We are done
      (and (nil? p) (nil? f))
      subs
      ; there are unmatched forms. 
      (nil? p)
      nil
      ; Match a list
      :else
      (let [p2 (first ps)]
        (cond
          ; ellipses. match each elment in forms, and reduce
          ; to one substitution
          ; TODO: failed matches will go unnoticed here!!
          (= p2 '...) 
          (if (empty? forms)
            (merge-concat subs {p nil})
            (apply merge-concat
                   (cons subs
                         (map #(iter (list %) (list p) {}) forms))))
          ; more forms than patterns
          (and (nil? p2) (not (empty? fs)))
          nil
          ; p is an identifier
          (symbol? p)
          (let [lit? (literal? p)]
            (cond
              ; f and p is the same literal, literals don't
              ; go into subs
              (and lit? (same-literal? p f)) 
              (recur fs ps subs)
              ; not the same literal
              (and lit? (not (same-literal? p f)))
              nil
              ; p is not a literal and the next p is not ... or .
              ; match. p should expand to f
              :else
              (recur fs ps (assoc subs p (list f)))))
          ; p is a list
          (list? p)
          (let [merged (merge-concat subs (iter f p {}))]
            (if merged (recur fs ps merged)))
          ; no matches
          :else nil))))
  
  (let [subs (iter exp-args 
                   pattern-vars 
                   {'... nil, macro-name (list macro-name)})]
    (if subs
      (list subs rewrite-rule);TODO return a vector
      nil))))
