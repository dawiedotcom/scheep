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
                             the-primitive-environment]]))

;;;; Forward declarations

(declare scheme-eval
         scheme-eval-fn
         scheme-apply
         scheme-load
         scheme-true?
         list-of-values
         the-global-environment
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
(defmethod eval-form 'set! [[_ variable value] env]
  (set-variable-value!
   env
   variable
   (scheme-eval value env))
  (fn [] 'ok))
  
;;;; Definitions
(defn procedure-definition? [variable]
  (list? variable))
(defn procedure-arguments [[_ & args]] args)
(defn procedure-name [[name]] name)
                                   
(defmethod eval-form 'define [[_ variable value] env]
  (let [name-clause variable]
    (if (procedure-definition? name-clause)
      (define-variable!
        env
        (procedure-name name-clause)
        (make-compound-procedure      
         (procedure-arguments name-clause)
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
(defn make-compound-procedure [params body env]
  ; this will have to do since we cannot declare record names..
  (compound-procedure. params body env))
(defn compound-procedure? [p] (instance? compound-procedure p))

;;;; Eval ;;;;
    
(defn list-of-values [exps env]
  (map #(scheme-eval % env) exps))


(defn scheme-eval-fn
  "Returns a fn of no arguments that returns the result
   of evaluating the scheme expression exp in the environent
   env"
  [exp env]
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
  (cond (primitive-procedure? procedure) (apply-primitive-procedure
                                          procedure
                                          arguments)
        (compound-procedure? procedure) (eval-sequence
                                         (.body procedure)
                                         (extend-environment
                                          (.env procedure)
                                          (.parameters procedure)
                                          arguments))
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
      (let [input-form (scheme-read-string input)
            output (scheme-eval input-form global-env)]
        (print-output output-prompt output)
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
