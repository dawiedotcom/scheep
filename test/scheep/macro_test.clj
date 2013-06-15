(ns scheep.macro-test
  (:use clojure.test 
        scheep.macro
        [scheep.core :only [scheme-eval setup-environment]]))

(deftest test-expand
  (testing "expanding let"
    (let [env (setup-environment)]
      (is (= (scheme-eval '(let ((a 1) (b 2)) b) env)
             (scheme-eval '((lambda (a b) b) 1 2) env))))))

(deftest test-syntax-rules
  (testing "Syntax rules"
    (let [exp '(syntax-rules ()
                             ((_ a b) (or a b))
                             ((_ a b c) (or a b c)))
          tau (syntax-rules exp (list))]
      (is (=
           (tau '(_ one two) (list))
           '(() {?b two, ?a one , ?_ _} (or ?a ?b))))
      (is (=
           (tau '(_ one two three) '())
           '(()
             {?c three, ?b two, ?a one,  ?_ _}
             (or ?a ?b ?c)))))))

;(expand-let-syntax '(let-syntax ((m  (syntax-rules () ((_ a b) (or a b))))) (m a b)) the-empty-environment)

(deftest test-let-syntax
  (let [env (setup-environment)]
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
        (is (= (scheme-eval exp env)
               (scheme-eval res env)))))
    (testing "example 2 from MTW"
      (let [exp '(let ((x "outer"))
                   (let-syntax ((m (syntax-rules ()
                                                 ((m)
                                                  x))))
                               (let ((x "inner"))
                                 (m))))
            res '(let ((x.1 "outer"))
                   (let ((x.2 "inner"))
                     x.1))]
        (is (= (scheme-eval exp env)
               (scheme-eval res env)))))
    (testing "example 3 from MTW"
      (let [exp '(let ((else false))
                   (cond (false 3)
                         (else 4)
                         (true 5)))
            res '(let ((else.1 false))
                   (if false (begin 3)
                       (if else.1 (begin 4)
                           (if true (begin 5) false))))]
        (is (= (scheme-eval exp env)
               (scheme-eval res env)))))))

(deftest test-expand-expression-no-marcos
  (testing "Expand expression with no marcos"
    (is (= (expand-expression 10 '()) 10))
    (is (= (expand-expression "hello" '()) "hello"))
    (is (= (expand-expression 'x '()) 'x))
    (is (= (expand-expression true '()) true))
    (is (= (expand-expression false '()) false))
    (is (= (expand-expression
            '(lambda (x) x)
            '())
           '(lambda (x) x)))))
