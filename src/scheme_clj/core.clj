(ns scheme-clj.core
  (:gen-class))

;;;; Forward declarations

(declare scheme-eval
         scheme-apply
         set-variable-value!
         define-variable!
         scheme-true?
         make-compound-procedure)
         
;;;; A map of eval function for scheme special forms

(def ^:dynamic *evals* (ref {}))

(defn get-form-symbol [exp] (keyword (first exp)))

(defn get-eval [exp]
  ((get-form-symbol exp) @*evals*))

(defn has-eval? [exp]
  (contains? @*evals* (get-form-symbol exp)))

(defn register-eval [symbol function]
  (dosync
   (alter *evals* #(assoc % symbol function))))

;;;; Expression Types ;;;;

(defn gen-selector [selector-symbol pos]
  ;; A clojure syntax function to define a selector using
  ;; destructuring
  `(~'defn ~selector-symbol [~(conj pos 's)] ~'s))

(defn gen-call-to-register-eval [symbol eval-fn]
  ;; A clojure syntax function to register the given eval
  ;; function in the map of evals
  `(register-eval ~(keyword symbol) ~eval-fn))

(defn tagged-list? [exp tag]
  (if (list? exp)
      (= (first exp) tag)
      false))

(defmacro defexpression
  "Defines predicates, selectors and evals for a scheme  
   special form.

   The call
     (defexpression :syntax (begin & begin-actions)
                    :eval (fn [exp env] ... ))
   will define the following functions:
    1) a predicate begin? to test if a scheme expression
       is the begin special form
    2) a selector begin-actions to get a list of the
       expressions inside begin.

   The value of :eval is an optional function that will
   be used automatically by scheme-eval to interpret the
   special form."
  [& args]
  (let [{[exp-symbol & selectors] :syntax
         eval-fn :eval} args]
    `(do
       ;; The predicate
       (~'defn ~(symbol (str exp-symbol "?")) [~'exp]
         (tagged-list? ~'exp (quote ~exp-symbol)))
       ;; Each of the selectors
       ~@(loop [[s & ss] selectors
                acc '()
                underscores '[_]]
           (cond
            (nil? s) acc
            (= s '&) (conj acc
                           ; Special case for & to select a list of all the
                           ; last forms
                           (gen-selector (first ss) (conj underscores '&)))
            :else (recur ss
                         (conj acc (gen-selector s underscores))
                         (conj underscores '_))))
       ;; Register the eval if one was given
       ~(if eval-fn (gen-call-to-register-eval exp-symbol eval-fn)))))

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
(defexpression :syntax (quote text-of-quotation))
  
;;; Assignment
(defexpression
  :syntax (set! assignment-variable assignment-value)
  :eval (fn [exp env]
          (set-variable-value!
           env
           (assignment-variable exp)
           (scheme-eval (assignment-value exp) env))
          'ok))
  
;;;; Definitions
(defexpression
  :syntax (define definition-variable definition-value)
  :eval (fn [exp env]
          (define-variable!
            env
            (definition-variable exp)
            (scheme-eval (definition-value exp) env))
          (definition-variable exp)))

;;;; Conditionals
(defexpression
  :syntax (if if-predicate if-consequent if-alternative)
  :eval (fn [exp env]
          (if (scheme-true? (scheme-eval (if-predicate exp) env))
            (scheme-eval (if-consequent exp) env)
            (scheme-eval (if-alternative exp) env))))

(defn make-if [predicate consequent alternative]
  (list 'if predicate consequent alternative))

;;;; Lambda expressions
(defexpression
  :syntax (lambda lambda-parameters & lambda-body)
  :eval (fn [exp env]
          (make-compound-procedure
           (lambda-parameters exp)
           (lambda-body exp)
           env)))

(defn make-lambda [params body]
  (list 'if params body))

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
        
(defexpression
  :syntax (begin & begin-actions)
  :eval (fn [exp env]
          (eval-sequence
           (begin-actions exp)
           env)))

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

;;;; Derived expressions ;;;;

;;;; Cond

(defexpression :syntax (cond & cond-clauses))

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
   (quote? exp) (text-of-quotation exp)
   (has-eval? exp) (let
                       [eval-proc (get-eval exp)]
                     (eval-proc exp env))
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
