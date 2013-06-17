(ns scheep.macro
  (:gen-class)
  (:use
   [clojure.core.unify :only [subst]]
   [clojure.walk :only [postwalk prewalk-replace]]
   [scheep.pattern-lang :only [match]]
   [scheep.primitives :only [pair?]]
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
;;
;; [1]: Clinger, Rees. 'Macros that work'
;;      http://dl.acm.org/citation.cfm?id=99607

;;;; Helper functions

(defn transcription? [id]
  (fn? id))

(defn lookup-transcription [symbol s-env]
  (let [denotation (lookup symbol s-env)]
    (if (and (symbol? denotation)
             (not= denotation symbol))
      (lookup-transcription denotation s-env)
      denotation)))
  
(defn macro-call? [exp s-env]
  (-> exp
      (first ,,,)
      (lookup-transcription ,,, s-env)
      (transcription? ,,,)))

(defn macro-definition? [[id]]
  (or (= id 'let-syntax)
      (= id 'define-syntax)))

(defn let-syntax? [[let-syn]]
  (= let-syn 'let-syntax))

(defn define-syntax? [[define-syn]]
  (= define-syn 'define-syntax))

(defn application? [exp]
  (list? exp))

(defn procedure-abstraction? [[lamb] s-env]
  (= (lookup lamb s-env) 'lambda))

(defn fresh-identifier [s-env id]
  (symbol (str id "." (count s-env))))

(defn self-evaluating? [exp]
  (or (string? exp)
      (nil? exp)
      (number? exp)
      (= true exp)
      (= false exp)))

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
    (expand-expression (first body) new-s-env)))

(defn define-syntax [[_ name transformer-spec] s-env]
  (let [transformer (syntax-rules transformer-spec s-env)]
    (bind s-env (list name) (list transformer))))

;;;; Macro expansion 
        
(defn expand-expression [exp s-env]
  (cond
   (self-evaluating? exp) exp
   (pair? exp) (seq exp)
   (symbol? exp) (lookup exp s-env)
   (procedure-abstraction? exp s-env) (expand-procedure exp s-env)
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

(defn rewrite [rule substitution s-env-def]
  (defn get-ids []
    (let [; flatten on a symbol returns ()
          rule- (if (list? rule) rule (list rule)) 
          unique-symbols (distinct (flatten rule-))
          pattern-vars (keys substitution)]
      (remove #(some #{%} pattern-vars)
              unique-symbols)))
  (let [identifiers (doall (get-ids))
        fresh-identifiers (map #(fresh-identifier s-env-def %) identifiers)
        fresh-ids-sub (zipmap identifiers fresh-identifiers)
        new-sub (merge substitution fresh-ids-sub)
        s-env-new (bind the-empty-environment
                        fresh-identifiers
                        (map #(lookup % s-env-def)
                             identifiers))
        dots-removed (postwalk
                      #(cond
                        ; Cons dots
                        (and (list? %)
                             (= (lookup (first %) s-env-new) '.))
                        (cons (second %) (nth % 2))
                        ; Replace the rule variable with the pattern
                        (contains? new-sub %)
                        (new-sub %)
                        ; Do nothing
                        :else %)
                      rule)]
    [dots-removed
     s-env-new]))
        
(defn transcribe [exp s-env-use]
  (let [macro-name (first exp)
        match (lookup-transcription (first exp) s-env-use)
                                        ;(lookup macro-name s-env-use)
        [s-env-def
         substitution
         rule] (match exp s-env-use)
        [transcribed-exp
         s-env-new] (rewrite rule substitution s-env-def)
        s-env-diverted (divert s-env-use s-env-new)]
    (expand-expression transcribed-exp s-env-diverted)))

;;;; Constructor for transformer functions 

(defn syntax-rules [[_ literals & patterns] s-env-def]
  ; Returns a function that when evaluated returns the
  ; list
  ;     (s-env-def substitution rewrite-rule)
  (fn [exp s-env-use]
    (let [matcher 
          (some #(match exp % literals)
                patterns)]
      (if matcher
        (conj matcher s-env-def)
        (throw 
          (ex-info "No matching pattern found:" {:cause exp}))))))

