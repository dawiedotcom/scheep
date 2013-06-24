(ns scheep.core
  (:gen-class)
  (:use
   [scheep.env :only [extend-environment
                      define-variable!
                      set-variable-value!
                      lookup-variable-value]]
   [scheep.reader :only [scheme-read-file
                         scheme-read-string]]
   [scheep.macro :only [expand]]
   [scheep.primitives :only [primitive-procedure?
                             apply-primitive-procedure
                             the-primitive-environment
                             pair?
                             make-pair]]))

;;;; Forward declarations

(declare scheme-eval
         scheme-eval-fn
         scheme-apply
         scheme-load
         scheme-true?
         list-of-values
         the-global-environment
         name-clause->params
         make-compound-procedure)
         
;;;; Expression Types ;;;;

(defmulti eval-form (fn [[op] env] op))

;;;; Self evaluating expressions

(defn self-evaluating? [exp]
  (or (string? exp)
      (nil? exp)
      (number? exp)
      (= true exp)
      (= false exp)))

;;;; Variables

(defn variable? [exp]
  (or (symbol? exp)
      (= exp true)
      (= exp false)))

;;;; Quotes
(defmethod eval-form 'quote [[_ text-of-quotation] env]
  (fn [] text-of-quotation))
  
;;; Assignment
(defmethod eval-form 'set! [[_ variable value :as exp] env]
  (if (self-evaluating? variable)
    (throw (ex-info "Cannot set! a constant:" {:cause exp})))
  (set-variable-value!
    env
    variable
    (scheme-eval value env))
  (fn [] 'ok))
  
;;;; Definitions
(defn procedure-definition? [variable]
  (or (pair? variable) (list? variable)))
    
(defn procedure-name [name-clause]
  (first (flatten (seq name-clause))))
                                   
(defmethod eval-form 'define [[_ variable value] env]
  (let [name-clause variable]
    (if (procedure-definition? name-clause)
      (define-variable!
        env
        (procedure-name name-clause)
        (make-compound-procedure      
         (name-clause->params variable)
         (list value)
         env))
      (define-variable!
        env
        variable
        (scheme-eval value env)))
    (fn [] 'ok)))

;;;; Conditionals
(defmethod eval-form 'if [[_ predicate consequent alternative] env]
  (if (scheme-true? (scheme-eval predicate env))
    (scheme-eval-fn consequent env)
    (scheme-eval-fn alternative env)))

;;;; Lambda expressions
(defmethod eval-form 'lambda [[_ parameters & body] env]
  #(make-compound-procedure
    parameters
    body
    env))

;;;; Begin

(defn last-exp? [exps] (empty? (rest exps)))

(defn eval-sequence [exps env]
  (loop [es exps]
    (if (last-exp? es)
      (scheme-eval-fn (first es) env)
      (do
        (scheme-eval (first es) env)
        (recur (rest es))))))

(defmethod eval-form 'begin [[_ & exprs] env]
  (eval-sequence exprs env))

;;;; Procedure application

(defn operator [[opr]] opr)
(defn operands [[_ & args]] args)

(defmethod eval-form :default [exp env]
  #(scheme-apply
    (scheme-eval (operator exp) env)
    (list-of-values (operands exp) env)))

;;;; Evaluator data strucures ;;;;

;;;; Truthyness

(defn scheme-true? [x]
  (not (= x false)))
(defn scheme-false? [x]
  (= x false))

;;;; Compound procedures

(defrecord compound-procedure [parameters body env])
(defn compound-procedure? [p] (instance? compound-procedure p))

(defn name-clause->params
  "Converts a scheme name clause to a parameter clause"
  [name-clause]
  (cond
   (list? name-clause) (rest name-clause)
   (pair? name-clause) (let [car (.car name-clause)]
                         (if (list? car)
                           (make-pair (rest car)
                                      (.cdr name-clause))
                           (make-pair '() (.cdr name-clause))))
   (symbol? name-clause) (make-pair '() name-clause)
   :else (throw
          (ex-info
           "Compound procedure name clause must be list, pair or symbol"
           {:cause name-clause}))))

(defn parameter-list
  "Returns a list of the symbol names in a procedure's arguments"
  [proc]
  (let [parameters (.parameters proc)]
    (cond (symbol? parameters) (list parameters)
          (pair? parameters) (if (empty? (.car parameters))
                               (list (.cdr parameters))
                               (flatten (seq parameters)))
          :else parameters)))

(defn argument-list
  "Returns the argument values in the same form as the argument
   list in a procedures definition"
  [proc args]
  (let [parameters (.parameters proc)]
    (cond
     ;; only one args that should be bound to the list of args
     (symbol? parameters) (list args) 
     (pair? parameters) (let [n (count (.car parameters))]
                          (apply
                           list
                           (concat (take n args)
                                   (list (nthnext args n)))))
     :else args)))
                              
(defn make-compound-procedure [param-clause body env]
  (let [params (if (symbol? param-clause)
                 (list param-clause)
                 (flatten (seq param-clause)))]
    (if-not (= (distinct params) params)
      (throw (ex-info "Procedure parameters must be unique:" 
                      {:cause (list 'lambda params '...)}))))
  (compound-procedure. param-clause body env))

;;;; Eval ;;;;
    
(defn list-of-values [exps env]
  (map #(scheme-eval % env) exps))


(defn scheme-eval-fn
  "Returns a fn of no arguments that returns the result
   of evaluating the scheme expression exp in the environent
   env"
  [exp env]
  (if (pair? exp)
    (throw (ex-info "Combination must be a proper list: " {:cause exp})))
  (let [expanded-exp (expand exp)]
    (cond 
      (self-evaluating? expanded-exp) (fn [] expanded-exp)
      (variable? expanded-exp) #(lookup-variable-value expanded-exp env)
      :else 
      (eval-form expanded-exp env))))

(defn scheme-eval
  "Evaluates the scheme expression exp in the environment env"
  [exp env]
  (trampoline scheme-eval-fn exp env))

;;;; Apply ;;;;

(defn scheme-apply
  "Applies a scheme procedure to a list of arguments"
  [procedure arguments]
  (cond
   ;; primitives
   (primitive-procedure? procedure)
   (apply-primitive-procedure
    procedure
    arguments)
   ;; compound procedures
   (compound-procedure? procedure)
   (eval-sequence
    (.body procedure)
    (extend-environment
     (.env procedure)
     (parameter-list procedure)
     (argument-list procedure arguments)))
   ;; nothing
   :else (throw
          (Exception. (str "Unknown procedure type " procedure "\n")))))
        
;;;; REPL ;;;;

;;;; The global environment
 
(defn setup-environment []
  (let [initial-env (the-primitive-environment)]
    (define-variable! initial-env 'true true)
    (define-variable! initial-env 'false false)
    (scheme-load "src/scheep/core.scm" initial-env)
    initial-env))

;;;; repl proper

(def input-prompt "; scheep> ")
(def output-prompt ";= ")

(defn prompt-for-input [prompt]
  (print (str "\n" prompt))
  (flush))
(defn user-str [output] (str output))
(defn print-output [prompt output]
  (println (str prompt (user-str output))))
  
(defn driver-loop [global-env]
  (prompt-for-input input-prompt)
  (let [input (read-line)] 
    (if input
      (let [input-form (scheme-read-string input)]
        (try
          (let [output (scheme-eval input-form global-env)]
            (print-output output-prompt output))
          (catch clojure.lang.ExceptionInfo ex 
            (println (str ";Error: "
                          (.getMessage ex)
                          (:cause (ex-data ex))))))
        (recur global-env))
      (println "\nbye."))))
    

(defn scheme-load [filename env]
  (let [exprs (scheme-read-file filename)]
    (doall (map #(scheme-eval % env) exprs))))

(def the-global-environment (setup-environment))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (driver-loop the-global-environment))
