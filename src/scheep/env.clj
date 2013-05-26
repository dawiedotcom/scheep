(ns scheep.env
  (:gen-class))

;;;; Provides lexical and syntactic environments for
;;;; the interpreter and the macro expander, respectivly.

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
  (try
    (with-var-in-env frame-get-binding exp env)
    (catch Exception ex (print-frame (first-frame env))
           (throw ex))))
(defn define-variable! [env var val] 
  (if (the-empty-environment? env)
      (throw (.Exception "Cannot define variables in the-empty-environment"))
      (let [frame (first-frame env)]
        (frame-add-binding! frame var val))))

