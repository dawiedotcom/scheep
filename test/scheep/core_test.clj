(ns scheep.core-test
  (:use clojure.test 
        scheep.core))

(deftest test-self-evaluating?
  (testing "Strings are self evaluating"
    (is (= (self-evaluating? "") true))
    (is (= (self-evaluating? "some other string") true)))
  (testing "Numbers are self evaluating"
    (is (= (self-evaluating? 10) true)))
  (testing "Some things that are not self evaluating"
    (not (= (self-evaluating? (fn [x] x))))))

(deftest primitive-procedure-application
  (let [env (setup-environment)]
    (testing "using primitive arithmatic procedures"
      (is (= (scheme-eval '(+ 2 8) env)
             10))
      (is (= (scheme-eval '(- 18 8) env)
             10))
      (is (= (scheme-eval '(* 2 5) env)
             10)))))

(deftest test-scheme-eval
  (let [env (setup-environment)]
    (testing "Eval strings"
      (is (= (scheme-eval "" env) ""))
      (is (= (scheme-eval "hello" env) "hello")))
    (testing "Eval numbers"
      (is (= (scheme-eval 10 env) 10))
      (is (= (scheme-eval (/ 2 3) env) (/ 2 3))))
    (testing "Eval begin"
      (is (= (scheme-eval '(begin 5 6) env)
             6))
      (is (= (scheme-eval '(begin 5) env)
             5)))
    (testing "Eval quote"
      (is (= (scheme-eval '(quote x) env) 'x))
      (is (= (scheme-eval ''x env) 'x)))
    ;(testing "Eval lambda"
    ;  (is (= (scheme-eval '(lambda (x) x) env)
    ;         (compound-procedure. '(x) '(x) env))))
    (testing "using arguments in procedures"
      (is (= (scheme-eval '((lambda (x) x) 5) env)
             5))
      (is (= (scheme-eval '((lambda (x y) x y) 5 6) env)
             6)))
    (testing "using globals in procedures"
      (is (= (scheme-eval '(begin (define x 10) ((lambda () x))) env)
             10)))
    (testing "using primitive procedures in application"
      (is (= (scheme-eval '((lambda (y) (+ y 5)) 5) env)
             10)))
    (testing "lexical scoping"
      (is (= (scheme-eval '(begin
                            (define x 100)
                            ((lambda (x) x) 10))
                          env)
             10)))))

(deftest test-set!
  (let [env (setup-environment)]
    (testing "set!"
      (is (= (scheme-eval
              '(begin
                (define x 100)
                (set! x 10)
                x)
              env)
             10)))))

(deftest test-define
  (let [env (setup-environment)]
    (testing "define self evaluating variables"
      (is (= (scheme-eval
              '(begin (define x 10) x)
              env)
             10))
      (is (= (scheme-eval
              '(begin (define x "this is a string") x)
              env)
             "this is a string")))
    (testing "define with expressions"
      (is (= (scheme-eval
              '(begin (define x (if false 20 10)) x)
              env)
             10))
      (is (= (scheme-eval
              '(begin (define x (quote test)) x)
              env)
             'test))
      (is (= (scheme-eval
              '(begin (define x (lambda (x y) (+ x y))) (x 5 5))
              env)
             10)))
    (testing "define syntax for procedures"
      (is (= (scheme-eval
              '(begin (define (id x) x)
                      (id 50))
              env)
             50))
      (is (= (scheme-eval
              '(begin (define (adder x y) (+ x y))
                      (adder (adder 10 10) 30))
              env)
             50)))))

(deftest test-cond
  (let [env (setup-environment)]
    (testing "Cond with only else"
      (is (= (scheme-eval
              '(cond (else 10))
              env)
             10)))
    (testing "simple cond"
      (is (= (scheme-eval
              '(cond (true 10) (else 100))
              env)
             10))
      (is (= (scheme-eval
              '(cond (false 5) (true 10) (else 100))
              env)
             10)))
    (testing "cond with procedures as predicates"
      (is (= (scheme-eval
              '(cond (((lambda () false)) 5)
                     (((lambda () true)) 10)
                     (else 100))
              env)
             10)))
    (testing "cond returning test with other clauses"
      (is (= (scheme-eval
              '(cond (10)
                     (((lambda () true)) 50)
                     (else 100))
              env)
             10)))
    (testing "cond returning test"
      (is (= (scheme-eval
              '(cond (10))
              env)
             10)))
    ;(testing "clauses after else"
    ;  (is (thrown-with-msg?
    ;       Exception
    ;       #"else clause.*last clause"
    ;       (scheme-eval
    ;        '(cond (else 10) (true 20))
    ;        env))))
    ))
 
(deftest test-let
  (let [env (setup-environment)]
    (testing "basic let"
      (is (= (scheme-eval '(let ((a 1)) a) env)
             1)))))
