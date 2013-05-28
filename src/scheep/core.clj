(ns scheep.core
  (:gen-class)
  (:use
   [scheep.env :only [extend-environment
                      the-empty-environment
                      the-empty-environment?
                      define-variable!
                      set-variable-value!
                      lookup-variable-value]]
   [scheep.reader :only [scheme-read-file]]
   [scheep.macro :only [expand]]))

;;;; Forward declarations

(declare scheme-eval
         scheme-apply
         scheme-load
         scheme-true?
         list-of-values
         make-compound-procedure)
         
;;;; Expression Types ;;;;

(defn tagged-list? [exp tag]
  (if (list? exp)
      (= (first exp) tag)
      false))
  
(defmulti eval-form (fn [[op] env] op))

;;;; Self evaluating expressions

(defn self-evaluating? [exp]
  (or (string? exp)
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
  text-of-quotation)
  
;;; Assignment
(defmethod eval-form 'set! [[_ variable value] env]
  (set-variable-value!
   env
   variable
   (scheme-eval value env))
  'ok)
  
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
    'ok))

;;;; Conditionals
(defmethod eval-form 'if [[_ predicate consequent alternative] env]
  (if (scheme-true? (scheme-eval predicate env))
    (scheme-eval consequent env)
    (scheme-eval alternative env)))

(defn make-if [predicate consequent alternative]
  (list 'if predicate consequent alternative))

;;;; Lambda expressions
(defmethod eval-form 'lambda [[_ parameters & body] env]
  (make-compound-procedure
   parameters
   body
   env))

;;;; Begin

(defn last-exp? [exps] (empty? (rest exps)))
(defn first-exp [[exp]] exp)
(defn rest-exp [[_ & rest]] rest)

(defn eval-sequence [exps env]
  (loop [es exps]
    (if (last-exp? es)
        (scheme-eval (first-exp es) env)
        (do
          (scheme-eval (first-exp es) env)
          (recur (rest-exp es))))))
        
(defmethod eval-form 'begin [[_ & exprs] env]
  (eval-sequence exprs env))

(defn make-begin [seq]
  (conj seq 'begin))

(defn sequence->exp [[first-exp & rest-exps]]
  ;; Wraps a sequence of expressions in a begin expression
  (cond
   (nil? first-exp) '()
   (nil? rest-exps) first-exp
   :else (make-begin seq)))
  
;;;; Procedure application

(defn application? [exp]
  (list? exp))

(defn operator [[opr]] opr)
(defn operands [[_ & args]] args)

(defn no-operands? [ops] (empty? ops))
(defn first-operand [[op]] op)
(defn rest-operands [[_ & ops]] ops)

(defmethod eval-form :default [exp env]
  (scheme-apply
    (scheme-eval (operator exp) env)
    (list-of-values (operands exp) env)))

;;;; Derived expressions ;;;;

;;;; Cond

(defn cond? [exp] (tagged-list? exp 'cond))
(defn cond-clauses [[_ & clauses]] clauses)

(defn cond-predicate [[pred]] pred)
(defn cond-actions [[_ & actions]] actions)
(defn cond-else-clause? [clause]
  (= (cond-predicate clause) 'else))

(defn expand-clauses [[first-clause & rest-clauses]]
  (if (nil? first-clause)
      'false                 ; cond with no else
      (if (cond-else-clause? first-clause)
          (if (empty? rest-clauses)
              (sequence->exp (cond-actions first-clause))
              (throw (Exception. "else clause isn't the last clause")))
          (make-if (cond-predicate first-clause)
                   (sequence->exp (cond-actions first-clause))
                   (expand-clauses rest-clauses)))))
  
(defn cond->if [exp]
  (expand-clauses (cond-clauses exp)))

;;;; Evaluator data strucures ;;;;

;;;; Truthyness

(defn scheme-true? [x]
  (not (= x false)))
(defn scheme-false? [x]
  (= x false))

;;;; Primitive procedures

(defrecord primitive-procedure [implementation])
(defn primitive-procedure? [p] (instance? primitive-procedure p))
(defn apply-primitive-procedure [proc args]
  (apply
   (.implementation proc)
   args))

;;;; Compound procedures

(defrecord compound-procedure [parameters body env])
(defn make-compound-procedure [params body env]
  ; this will have to do since we cannot declare record names..
  (compound-procedure. params body env))
(defn compound-procedure? [p] (instance? compound-procedure p))

;;;; Eval ;;;;
    
(defn list-of-values [exps env]
  (loop [es exps
         acc (list)]
    (if (no-operands? es)
        (reverse acc)
        (recur (rest-operands es)
               (cons (scheme-eval (first-operand es) env)
                     acc)))))

(defn scheme-eval [exp env]
  (cond 
    (self-evaluating? exp) exp
    (variable? exp) (lookup-variable-value exp env)
    (cond? exp) (scheme-eval
                  (cond->if exp)
                  env)
    :else (eval-form exp env)))

;;;; Apply ;;;;

(defn scheme-apply [procedure arguments]
  ;(println (str "scheme-apply arguments:" arguments))
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

(def primitive-procedures
  (list (list 'car first)
        (list 'cdr rest)
        (list 'null? empty?)
        (list 'cons cons)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '> >)
        (list '< <)
        (list 'load scheme-load)
        (list 'list list)
        (list 'display print)
        (list 'newline (fn [] (println)))))

(defn primitive-procedure-names [] (map first primitive-procedures))
(defn primitive-procedure-objects []
  (map (fn [[_ p]] (primitive-procedure. p))
       primitive-procedures))
  
(defn setup-environment []
  (let [initial-env
        (extend-environment
         the-empty-environment
         (primitive-procedure-names)
         (primitive-procedure-objects))]
    (define-variable! initial-env 'true true)
    (define-variable! initial-env 'false false)
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
  (let [input (read *in* false nil)]
    (if (or input (false? input))
      (let [[expanded] (expand (list input))
            output (scheme-eval expanded global-env)]
        ;(println expanded)
        (print-output output-prompt output)
        (recur global-env))
      (println))))
    
(def the-global-environment (setup-environment))

(defn scheme-load [filename]
  (let [env the-global-environment
        expanded-exprs (->>
                         filename
                         (scheme-read-file)
                         (expand))]
    (map #(scheme-eval % env) expanded-exprs)))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (scheme-load "src/scheep/core.scm")
  (driver-loop the-global-environment))
