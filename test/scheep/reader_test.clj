(ns scheep.reader-test
  (:use clojure.test 
        scheep.reader))


(deftest test-scheme-read-string
  (testing "numbers"
    (is (= (scheme-read-string "5") 5))
    (is (= (scheme-read-string "05") 5))
    (is (= (scheme-read-string "-5") -5)))
  (testing "strings"
    (is (= (scheme-read-string "\"hello\"") "hello")))
  (testing "symbols"
    (is (= (scheme-read-string "a-symbol") 'a-symbol))
    (is (= (scheme-read-string "set!") 'set!))
    (is (= (scheme-read-string "!$%&*+-./:<=>?@^_~") 
           (symbol "!$%&*+-./:<=>?@^_~"))))
  (testing "lists"
    (is (= (scheme-read-string "(a b c)") '(a b c)))
    (is (= (scheme-read-string "(a   b   \n  c)") '(a b c)))
    (is (= (scheme-read-string "()") '()))
    (is (= (scheme-read-string "(a (b c))") '(a (b c))))
    (is (= (scheme-read-string "(a (b c) d)") '(a (b c) d))))
  (testing "dotted-lists"
    (is (= (scheme-read-string "(a . b)") '(a b))))
  (testing "comments with ;"
    (is (= (scheme-read-string "(a   ; a as first\n b \n c)") '(a b c))))
  (testing "peculiar identifiers"
    (is (= (scheme-read-string "...") '...))
    (is (= (scheme-read-string "(a ...)") '(a ...)))
    (is (= (scheme-read-string "-") '-))
    (is (= (scheme-read-string "+") '+)))
  (testing "booleans"
    (is (= (scheme-read-string " #t") true))
    (is (= (scheme-read-string " #f") false)))
  (testing "quote etc."
    (is (= (scheme-read-string "'x") '(quote x)))
    (is (= (scheme-read-string "`x") '(quasiquote x)))
    (is (= (scheme-read-string ",x") '(unquote x)))
    (is (= (scheme-read-string ",@x") '(unquote-splicing x)))))
