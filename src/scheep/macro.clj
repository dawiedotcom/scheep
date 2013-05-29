(ns scheep.macro
  (:gen-class)
  (:use
   [scheep.env :only [the-empty-environment
                      the-empty-environment?
                      lookup
                      bind
                      divert]]))
                      

(declare syntax-rules
         expand-expression
         transcribe)

;;;; Macros ;;;;
;; A very direct implementation of 'Macros That work' [1].
;; TODO:
;;  * define-syntax and other variations of let-syntax
;;  * all variations of the pattern lanuage for syntax-rules
;;
;; [1]: Clinger, Rees. 'Macros that work'
;;      http://dl.acm.org/citation.cfm?id=99607

;;;; Helper functions

(defn transcription? [id]
  (fn? id))

(defn macro-call? [exp s-env]
  (transcription? (lookup (first exp) s-env)))

(defn macro-definition? [[id]]
  (or (= id 'let-syntax)
      (= id 'define-syntax)))

(defn let-syntax? [[let-syn]]
  (= let-syn 'let-syntax))

(defn define-syntax? [[define-syn]]
  (= define-syn 'define-syntax))

(defn application? [exp]
  (list? exp))

(defn procedure-abstraction? [[lamb]]
  (= lamb 'lambda))

(defn fresh-identifier [s-env id]
  (symbol (str id "." (count s-env))))

;;;; The global syntactic environment - so that consecutive calls
;;;; to expand can use the same macros defined with define-syntax.

(def ^:dynamic *global-s-env* (atom the-empty-environment))
(defn update-global-env! [new-env]
  (reset! *global-s-env* new-env))

;;;; Expand lists

(defn expand-expression-list [exps s-env]
  ;; Recursively expand a list of expressions
  (loop [es exps
         acc (list)]
    (if (empty? es)
      (reverse acc)
      (recur (rest es)
             (cons (expand-expression (first es) s-env)
                   acc)))))

;;;; Expand procedures and lambdas

(defn expand-application [exp s-env]
  (expand-expression-list exp s-env))

(defn make-lambda [params & body]
  (cons 'lambda (cons params body)))

(defn expand-procedure [[_ args & body] s-env]
  (if (the-empty-environment? s-env)
    (apply make-lambda (cons args body))
    (let [fresh-args (map #(fresh-identifier s-env %) args)
          body-s-env (bind s-env args fresh-args)
          expanded-body (expand-expression-list body body-s-env)]
      (apply
       make-lambda 
       (cons
        fresh-args
        expanded-body)))))
                         
;;;; Expand macro definitions

(defn expand-let-syntax [[_ bindings & body] s-env]
  (let [macro-names (map first bindings)
        s-rule-clauses (map (fn [[_ c]] c) bindings)
        transformers (map #(syntax-rules % s-env) s-rule-clauses)
        new-s-env (bind s-env macro-names transformers)]
    ;(pprint bindings)
    (expand-expression (first body) new-s-env)))

(defn define-syntax [[_ name transformer-spec] s-env]
  (let [transformer (syntax-rules transformer-spec s-env)]
    (bind s-env (list name) (list transformer))))

;;;; Macro expansion 
        
(defn expand-expression [exp s-env]
  (cond
   (or (number? exp)
       (true? exp)
       (false? exp)) exp
   (symbol? exp) (lookup exp s-env)
   (procedure-abstraction? exp) (expand-procedure exp s-env)
   (let-syntax? exp) (expand-let-syntax exp s-env)
   (macro-call? exp s-env) (transcribe exp s-env)
   (application? exp) (expand-application exp s-env)
   :else exp))

(defn expand [exp]
  ;; The top level marco expander
  (if (and (coll? exp) (define-syntax? exp))
    (let [new-glob (define-syntax exp @*global-s-env*)]
      (update-global-env! new-glob)
      nil)
    (expand-expression exp @*global-s-env*)))

;;;; The hygenic macro expansion functions from [1]

(defn merge-concat [& maps]
  ;; assuming each map has only lists as values, this
  ;; does the same as merge, but concats vals that
  ;; correspond to the same key.
  (defn reducer [m1 m2]
    (if (and m1 m2)
      (let [ks (concat (keys m1) (keys m2))
            vs (map #(concat (% m1) (% m2)) ks)]
        (zipmap ks vs))))
  (reduce reducer maps))


(defn rewrite [rule substitution s-env-def]
  (defn get-ids []
    (let [unique-symbols (distinct (flatten rule))
          pattern-vars (keys substitution)]
      (remove #(some #{%} pattern-vars)
              unique-symbols)))
  (let [identifiers (doall (get-ids))
        fresh-identifiers (map #(fresh-identifier s-env-def %)
                               identifiers)
        fresh-ids-sub (zipmap identifiers
                              (map list fresh-identifiers))
        new-sub (merge-concat substitution fresh-ids-sub)
        s-env-new (bind the-empty-environment
                        fresh-identifiers
                        (map #(lookup % s-env-def)
                             identifiers))]
    (defn rewrite-h [exp rewritten]
      (cond
       ; exp is a symbol
       (symbol? exp)
       (first (exp new-sub))
       ; no more forms left in the pattern's rule
       (empty? exp)
       (apply list rewritten)
       ; expand recursively into a list 
       (list? (first exp))
       (recur
        (rest exp)
        (concat rewritten
                (vector
                 (rewrite-h (first exp) (vector)))))
       ; expand the next form in the rule
       :else (recur
              (rest exp)
              (concat  rewritten
                       ((first exp) new-sub)))))
    [(rewrite-h rule (vector))
     s-env-new]))
        
(defn transcribe [exp s-env-use]
  (let [macro-name (first exp)
        match (lookup macro-name s-env-use)
        [s-env-def
         substitution
         rule] (match exp s-env-use)
        [transcribed-exp
         s-env-new] (rewrite rule substitution s-env-def)
        s-env-diverted (divert s-env-use s-env-new)]
    (expand-expression transcribed-exp s-env-diverted)))

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
      nil)))
  
;;;; Constructor for transformer functions 

(defn syntax-rules [[_ literals & patterns] s-env-def]
  ; Returns a function that when evaluated returns the
  ; list
  ;     (s-env-def substitution rewrite-rule)
  (fn [exp s-env-use]
    (let [matcher 
          (some #(match exp
                        %
                        literals
                        s-env-use
                        s-env-def)
                patterns)]
      (if matcher
        (conj matcher s-env-def)
        (throw 
          (Exception. 
            (str "No matching pattern found: " literals " => " patterns)))))))

