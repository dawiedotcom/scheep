(ns scheep.core-test
  (:use clojure.test 
        scheep.core
        scheep.reader))

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

(deftest test-and
  (let [env (setup-environment)]
    (testing "and without args"
      (is (= (scheme-eval '(and) env) true)))
    (testing "and with one arg"
      (is (= (scheme-eval '(and false) env) false))
      (is (= (scheme-eval '(and 10) env) 10)))
    (testing "and with more than one arg"
      (is (= (scheme-eval '(and 5 10) env) 10))
      (is (= (scheme-eval '(and 5 false) env) false))
      (is (= (scheme-eval '(and 5 false 10) env) false))
      (is (= (scheme-eval '(and 5 8 10) env) 10)))))
  
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

    (testing "cond with => as last clause"
      (is (= (scheme-eval
              '(cond (10 => (lambda (x) (+ 10 x))))
              env)
             20)))

    (testing "cond with clauses following =>"
      (is (= (scheme-eval
              '(cond (false => (lambda (x) (+ 10 x)))
                     (true 20))
              env)
             20))
      (is (= (scheme-eval
              '(cond (10 => (lambda (x) (+ 10 x)))
                     (true 10))
              env)
             20)))
    ;
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
             1))
      (is (= (scheme-eval '(let ((a 10) (b 1)) b) env)
             1)))))

(deftest test-let*
  (let [env (setup-environment)]
    (testing "basic let*"
      (is (= (scheme-eval '(let* ((a 1)) a) env)
             1))
      (is (= (scheme-eval '(let* ((a 10) (b a)) b) env)
             10)))))

(deftest test-macro-hygene
  (let [env (setup-environment)]
    (testing "let with val and name"
      (is (= (scheme-eval
              '(let ((val 10) (name "dave")) val)
              env)
             10)))
    ;(testing "let from MTW"
    ;  (is (= (scheme-eval
    ;          '(let-syntax ((push (syntax-rules ()
    ;                                ((push v x)
    ;                                 (set! x (cons v x))))))
    ;             (let ((pros (list "cheap" "fast"))
    ;                   (cons (list)))
    ;               (push "unreliable" cons)
    ;               cons))
    ;          env)
    ;         "unreliable")))
              ))

(deftest test-tco
  (testing "tco to 10000"
    (is (= (scheme-eval
            '(begin
              (define (times n)
                (if (= n 0)
                  0
                  (times (- n 1))))
              (times 10000))
            the-global-environment)
           0))))

(deftest test-lambda-variations
  (testing "define with one . args"
    (is (= (scheme-eval
            (scheme-read-string
             "(begin (define (foo . args) args) (foo 1 2 3))")
            the-global-environment)
           '(1 2 3))))
  (testing "define with mixed args"
    (is (= (scheme-eval
            (scheme-read-string
             "(begin (define (foo x . args) args) (foo 1 2 3))")
             the-global-environment)
           '(2 3)))
    (is (= (scheme-eval
            (scheme-read-string
             "(begin (define (foo x . args) x) (foo 1 2 3))")
            the-global-environment)
           1)))
  (testing "lambda with . args"
    (is (= (scheme-eval
            (scheme-read-string
             "(begin (define foo (lambda (x . args) args)) (foo 1 2 3))")
            the-global-environment)
           '(2 3)))
    (is (= (scheme-eval
            (scheme-read-string
             "(begin (define foo (lambda (x . args) x)) (foo 1 2 3))")
            the-global-environment)
           1)))
   (testing "lambda with symbol as args"
    (is (= (scheme-eval
            (scheme-read-string
             "(begin (define foo (lambda args args)) (foo 1 2 3))")
            the-global-environment)
           '(1 2 3)))))          
