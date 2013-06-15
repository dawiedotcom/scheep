(ns scheep.macro
  (:gen-class)
  (:use
   [clojure.core.unify :only [subst]]
   [scheep.pattern-lang :only [?+ get-pattern pattern merge-concat]]
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
  #_(println "expand-procedure: (lambda " args body ")")
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
  #_(println "expand-expression: " exp "\n")
  (cond
   (self-evaluating? exp) exp
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
    (let [rule- (if (list? rule) rule (list rule))
          unique-symbols (distinct (flatten rule-))
          pattern-vars (keys substitution)]
      #_(println "get-ids: \n"
               "  unique-symbols: " unique-symbols "\n")
      (remove #(some #{%} pattern-vars)
              unique-symbols)))
  (let [identifiers (doall (get-ids))
        fresh-identifiers (map #(fresh-identifier s-env-def %)
                               identifiers)
        fresh-ids-sub (zipmap identifiers
                              ;(map list fresh-identifiers))
                              fresh-identifiers)
        ;new-sub (merge-concat substitution fresh-ids-sub)
        new-sub (merge substitution fresh-ids-sub)
        s-env-new (bind the-empty-environment
                        fresh-identifiers
                        (map #(lookup % s-env-def)
                             identifiers))
        subst-workaround (into {}
                              (filter (fn [[_ v]] (or (nil? v) (false? v)))
                                      new-sub))]
    #_(println "rewrite: \n"
             "  identifiers: " identifiers "\n"
             "  fresh-ids-sub: " fresh-ids-sub "\n"
             "  new-sub: " new-sub "\n"
             "  rule:" rule "\n"
             "  substitution: " substitution "\n"
             "  s-env-new: " s-env-new "\n")
    [(clojure.walk/prewalk-replace new-sub rule)
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
    #_(println "transcribe: \n"
             "  transcribed-exp: " transcribed-exp "\n"
             "  s-env-diverted: " s-env-diverted "\n")
    ;expand-expression))
    (expand-expression transcribed-exp s-env-diverted)))

(defn match [[macro-name & exp-args :as exp]
             [[_ & pattern-vars] rewrite-rule]
             literals
             s-env-use
             s-env-def]
  (let [subs (pattern {:form exp-args
                       :exp exp
                       :pattern pattern-vars
                       :literals literals
                       :use-env s-env-use
                       :def-env s-env-def
                       :acc {macro-name (list macro-name)}})]
    (if subs
      (list subs rewrite-rule))))
  
;;;; Constructor for transformer functions 

(defn syntax-rules [[_ literals & patterns] s-env-def]
  ; Returns a function that when evaluated returns the
  ; list
  ;     (s-env-def substitution rewrite-rule)
  (fn [exp s-env-use]
    (let [matcher 
          (some #(get-pattern exp
                              %
                              literals
                              s-env-use
                              s-env-def)
                patterns)

          #_(some #(match exp
                        %
                        literals
                        s-env-use
                        s-env-def)
                patterns)]
      (if matcher
        (conj matcher s-env-def)
        (throw 
          (ex-info "No matching pattern found:" {:cause exp}))))))

