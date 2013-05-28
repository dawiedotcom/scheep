(ns scheep.env
  (:gen-class))

;;;; Provides lexical and syntactic environments for
;;;; the interpreter and the macro expander, respectively.

;;;; Frames 

(defn make-frame [variables values]
  (atom (zipmap variables values)))
(defn frame-add-binding! [frame var val]
  (reset! frame (assoc @frame var val)))
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

(defn with-var-in-env [proc var env not-found-proc] 
  (letfn [(lookup [e]
            (if (the-empty-environment? e)
                (not-found-proc var)
                (let [frame (first-frame e)]
                  (if (frame-defined? frame var)
                      (proc frame var)
                      (recur (enclosing-environment e))))))]
    (lookup env)))

(defn throw-var-not-found [var]
  (throw (Exception.
          (str "Variable " var " is not defined."))))

;;;; Lexical environments - for the evaluator

(defn set-variable-value! [env var val]
  (with-var-in-env
    #(frame-add-binding! %1 %2 val)
    var
    env
    throw-var-not-found))
(defn lookup-variable-value [exp env]
  ;; Called by eval, only if exp is a variable
  (with-var-in-env frame-get-binding exp env throw-var-not-found))
(defn define-variable! [env var val] 
  (if (the-empty-environment? env)
      (throw
       (.Exception
        "Cannot define variables in the-empty-environment"))
      (let [frame (first-frame env)]
        (frame-add-binding! frame var val))))

;;;; Syntactic environments - for the macro expander

(defn lookup [id s-env]
  (with-var-in-env frame-get-binding id s-env identity))

(defn bind [s-env symbols bindings]
  (extend-environment s-env symbols bindings))

(defn divert [env1 env2]
  (concat env1 env2))

