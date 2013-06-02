(ns scheep.reader
  (:gen-class)
  (:use
   [clojure.core :exclude [read-string]]
   [blancas.kern.core :only [<|> <+> >>= run run* alpha-num one-of* value
                             many1 digit sep-by white-space bind skip-ws
                             return fwd]]
   [blancas.kern.lexer.basic :only [string-lit parens]]))

;;;; A scheme reader

(declare scheme-parser
         bind-ctor
         symbol-
         number-
         list-
         )

(defn- str-rest [s]
  (apply str (rest s)))

(defn scheme-read-string [string]
  "Same as core/read-string, but reads a scheme form"
  (value scheme-parser string))
  
(defn parse-forms [in-string]
  ;; Parses a list of forms from one input string.
  (loop [in-string in-string
	 form-so-far ""
	 count 0
	 res (list)]
    (if (empty? in-string)
      (reverse res)
      (let [s (first in-string)
            ss (str-rest in-string)
	    c (cond (= s \)) (- count 1)
		    (= s \() (+ count 1)
		    :else count)]
	(cond (= s \newline) (recur ss
                                    form-so-far
                                    count
                                    res)
              (= c 0) (recur ss
                             ""
                             0
                             (cons (str form-so-far s) res))
              :else (recur ss
                           (str form-so-far s)
                           c
                           res))))))

(defn scheme-read-file 
  "Reads a list of scheme forms from the given input file"
  [filename]
  (->>
   filename
   (slurp)
   (parse-forms)
   (map read-string)))

;;; Symbols

(def special-char
  (one-of* "!$%&*+-./:<=>?@^_~"))

(def symbol= (many1 (<|> alpha-num special-char)))

;;; Numbers

(def number= (many1 digit))

;;; Lists


(def list-elems= (sep-by white-space scheme-parser))

#_(def dotted-list-elms= (bind [car scheme-parser
                              _ (one-of \.)
                              cdr scheme-parser]
                             (return (list car cdr))))
  
(def list= (parens list-elems=))
                   

;;; Helpers

(defn bind-ctor [p ctor]
  (bind [str (skip-ws (<+> p))]
        (return (ctor str))))

(def number- (bind-ctor number= (fn [s] (Integer/parseInt s))))
(def symbol- (bind-ctor symbol= symbol))
;(def list- (bind-ctor (parens list=) #(apply list %)))
(def list- (>>= list= #(return (apply list %))))
;(def string- (bind-ctor string= identity))

;;; The parser

(def scheme-parser
  (<|> list- number- symbol- string-lit))

