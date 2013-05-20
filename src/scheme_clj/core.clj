(ns scheme-clj.core
  (:gen-class))

;;;; Forward declarations

(declare scheme-eval
         scheme-apply)

;;;; Expression Types ;;;;

(defmacro defexpression [[exp-symbol & selectors]]
  (defn defselector [selector-symbol pos]
    `(~'defn ~selector-symbol [~(conj pos 's)] ~'s))
  ;(println (str "defexpression: " exp-symbol selectors))
  `(do
     (~'defn ~(symbol (str exp-symbol "?")) [~'exp]
       (tagged-list? ~'exp (quote ~exp-symbol)))
     ~@(loop [[s & ss] selectors
              acc '()
              underscores '[_]]
         (cond
          (nil? s) acc
          (= s '&) (conj acc
                         (defselector (first ss) (conj underscores '&)))
          :else (recur ss
                       (conj acc (defselector s underscores))
                       (conj underscores '_))))))

(defn tagged-list? [exp tag]
  (if (list? exp)
      (= (first exp) tag)
      false))

;;;; Self evaluating expressions

(defn self-evaluating? [exp]
  (or (string? exp)
      (number? exp)))

;;;; Variables

(defn variable? [exp]
  (or (symbol? exp)
      (= exp true)
      (= exp false)))

;;;; Quotes
(defexpression (quote text-of-quotation))
  
;;; Assignment
(defexpression (set! assignment-variable assignment-value))
(def assignment? set!?)

;;;; Definitions
(defexpression (define definition-variable definition-value))
(def definition? define?)

;;;; Conditionals
(defexpression (if if-predicate if-consequent if-alternative))

(defn make-if [predicate consequent alternative]
  (list 'if predicate consequent alternative))

;;;; Lambda expressions
(defexpression (lambda lambda-parameters & lambda-body))

(defn make-lambda [params body]
  (list 'if params body))

;;;; Begin
(defexpression (begin & begin-actions))

(defn make-begin [seq]
  (conj seq 'begin))

(defn last-exp? [exps] (empty? (rest exps)))
(defn first-exp [[exp]] exp)
(defn rest-exp [[_ & rest]] rest)

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

;;;; Derived expressions ;;;;

;;;; Cond

(defexpression (cond & cond-clauses))

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
(defn compound-procedure? [p] (instance? compound-procedure p))

;;;; Frames 

(defn make-frame [variables values]
  (ref (zipmap variables values)))
(defn frame-add-binding! [frame var val]
  (dosync
   (alter frame #(assoc % var val))))
(defn frame-get-binding [frame var]
  (@frame var))
(defn frame-defined? [frame var]
  (contains? @frame var))

(defn print-frame [frame]
  (println)
  (doseq [[var val] @frame]
    (println (str "\t" var " => " val)))
  (println))

;;;; Environments 

(def the-empty-environment nil)
(defn the-empty-environment? [env]
  (empty? env))

(defn first-frame [env] (first env))
(defn enclosing-environment [env] (next env))
(defn extend-environment [env vars vals] 
  (conj env (make-frame vars vals)))

(defn with-var-in-env [proc var env] 
  (letfn [(lookup [e]
            (if (the-empty-environment? e)
                (Exception. (str "Variable " var " is not defined\n"))
                (let [frame (first-frame e)]
                  ;(println (str "Looking for " var " in:"))
                  ;(print-frame frame)
                  (if (frame-defined? frame var)
                      (proc frame var)
                      (recur (enclosing-environment e))))))]
    (lookup env)))

(defn set-variable-value! [env var val]
  (with-var-in-env #(frame-add-binding! %1 %2 val) var env))
(defn lookup-variable-value [exp env]
  ;; Called by eval, only if exp is a variable
  (with-var-in-env frame-get-binding exp env))
(defn define-variable! [env var val] 
  (if (the-empty-environment? env)
      (throw (.Exception "Cannot define variables in the-empty-environment"))
      (let [frame (first-frame env)]
        (frame-add-binding! frame var val))))

;;;; Eval ;;;;

(defn eval-assignment [exp env]
  (set-variable-value!
    env
    (assignment-variable exp)
    (scheme-eval (assignment-value exp) env))
  'ok)


(defn eval-definition [exp env]
  (define-variable!
    env
    (definition-variable exp)
    (scheme-eval (definition-value exp) env))
  (definition-variable exp))
    
(defn eval-if [exp env]
  (if (scheme-true? (scheme-eval (if-predicate exp) env))
      (scheme-eval (if-consequent exp) env)
      (scheme-eval (if-alternative exp) env)))
    
(defn list-of-values [exps env]
  (loop [es exps
         acc (list)]
    (if (no-operands? es)
        (reverse acc)
        (recur (rest-operands es)
               (cons (scheme-eval (first-operand es) env)
                     acc)))))

(defn eval-sequence [exps env]
  ;(println env)
  (loop [es exps]
    (if (last-exp? es)
        (scheme-eval (first-exp es) env)
        (do
          (scheme-eval (first-exp es) env)
          (recur (rest-exp es))))))
        
(defn scheme-eval [exp env]
  (cond 
    (self-evaluating? exp) exp
    (variable? exp) (lookup-variable-value exp env)
    (quote? exp) (text-of-quotation exp)
    (assignment? exp) (eval-assignment exp env)
    (definition? exp) (eval-definition exp env)
    (if? exp) (eval-if exp env)
    (lambda? exp) (compound-procedure.
                   (lambda-parameters exp)
                   (lambda-body exp)
                   env)
    (begin? exp) (eval-sequence
                  (begin-actions exp)
                  env)
    (cond? exp) (scheme-eval
                 (cond->if exp)
                 env)
    (application? exp) (scheme-apply
                        (scheme-eval (operator exp) env)
                        (list-of-values (operands exp) env))
    :else (throw
           (Exception. (str "Unknown expression type " exp "\n")))))

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

(def input-prompt "; scheme-clj> ")
(def output-prompt ";= ")

(defn prompt-for-input [prompt]
  (print (str "\n" prompt))
  (flush))
(defn user-str [output] (str output))
(defn print-output [prompt output]
  (println (str prompt (user-str output))))
  
(defn driver-loop [global-env]
  (prompt-for-input input-prompt)
  (let [input (read)
        output (scheme-eval input global-env)]
    (print-output output-prompt output)
    ;(user-print output))
    (recur global-env)
    ))
    
(def the-global-environment (setup-environment))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (driver-loop the-global-environment))
