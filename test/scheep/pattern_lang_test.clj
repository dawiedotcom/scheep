(ns scheep.pattern-lang-test
  (:use clojure.test 
        scheep.pattern-lang))


;;; Test symbol matching

(deftest symbol-pattern-test
  (let [simple-map {:literals '() :def-env '() :use-env '()}]
    (testing "simple symbols"
      (is (= (symbol-pattern
              (assoc simple-map :form '(arg1) :pattern '(p1)))
             '{p1 (arg1)}))

      (is (= (symbol-pattern
              (assoc simple-map :form '(arg1 arg2) :pattern '(p1 p2)))
             '{p1 (arg1), p2 (arg2)})))

    (testing "patterns with self evaluating values"
      (is (= (symbol-pattern
              (assoc simple-map :form '(10 "s" 'a) :pattern '(p1 p2 p3)))
             '{p1 (10) p2 ("s") p3 ('a)})))

    (testing "patterns with the same names as forms"
      (is (= (symbol-pattern
              (assoc simple-map :form '(a b) :pattern '(a b)))
             '{a (a) b (b)})))

    (testing "with too many args in the form"
      (is (= (symbol-pattern
              (assoc simple-map :form '(arg1 arg2) :pattern '(p1)))
             nil))))

  (testing "with literal expressions"
    (let [simple-map {:literals '(else) :def-env '() :use-env '()}]
      (is (= (symbol-pattern
              (assoc simple-map
                :form '(else)
                :pattern '(else)
                :literals '(else)))
             nil))

      (is (= (symbol-pattern
              (assoc simple-map
                :form '(else arg1)
                :pattern '(else p1)
                :literals '(else)))
             '{p1 (arg1)})))))

(deftest test-concrete-pattern
  (testing "recursive patterns"
    (let [simple-map {:literals '() :def-env '() :use-env '() :acc {}}]
      (is (= (concrete-pattern
              (assoc simple-map
                :pattern '((var val))
                :form '((a b))))
             '{var (a) val (b)})))))

(deftest test-elipsis
  (testing "let like binding"
    (let [simple-map {:literals '() :def-env '() :use-env '() :acc {}}]
      (is (= (pattern
              (assoc simple-map
                :pattern '((var val) ...)
                :form '((a b) (c d))))
             '{var (a c) val (b d) ... ()})))))

;(deftest test-let
  ;(testing "
