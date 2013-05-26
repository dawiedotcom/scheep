(ns scheep.macro-test
  (:use clojure.test 
        scheep.macro))

(deftest test-match
  (testing "Match with exp and pattern of the same length"
    (let [pattern '((_ a b) (or a b))
          [subs rule] (match '(or2 one two)
                             pattern
                             '()
                             '()
                             '())]
      (is (and (= subs {'a 'one, 'b 'two})
               (= rule '(or a b))))
      (is (= (match '(or2 one two three)
                    pattern
                    '()
                    '()
                    '())
             nil)))))


(deftest test-syntax-rules
  (testing "Syntax rules"
    (let [exp '(syntax-rules ()
                             ((_ a b) (or a b))
                             ((_ a b c) (or a b c)))
          tau (syntax-rules exp '(def-env))]
      (is (=
           (tau '(_ one two) '(use-env))
           '((def-env) {b two, a one} (or a b))))
      (is (=
           (tau '(_ one two three) '(use-env))
           '((def-env)
             {c three, b two, a one}
             (or a b c)))))))

;(expand-let-syntax '(let-syntax ((m  (syntax-rules () ((_ a b) (or a b))))) (m a b)) the-empty-environment)

(deftest test-let-syntax
  (testing "example 1 from MTW"
    (let [exp '(let-syntax
                ((push (syntax-rules ()
                                     ((push v x)
                                      (set! x (cons v x))))))
                ((lambda (pros cons)
                         (push "unreliable" cons)
                         cons)
                 (list "cheap" "fast")
                 (list)))
          res '((lambda (pros.1 cons.1)
                        (set! cons.1 (cons "unreliable" cons.1))
                        cons.1)
                (list "cheap" "fast")
                (list))]
      ; Should maybe check if the expanded exp and res evals to
      ; the same thing
      (is (= (expand-expression exp '())
             res)))))
