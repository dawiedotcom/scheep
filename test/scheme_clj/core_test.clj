(ns scheme-clj.core-test
  (:use clojure.test 
        scheme-clj.core))

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
    (testing "clauses after else"
      (is (thrown-with-msg?
           Exception
           #"else clause.*last clause"
           (scheme-eval
            '(cond (else 10) (true 20))
            env))))))
  
