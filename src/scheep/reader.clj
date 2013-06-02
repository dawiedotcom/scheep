(ns scheep.reader
  (:gen-class)
  (:use
   [clojure.core :exclude [read-string]]
   [blancas.kern.core :only [<|> <:> <+> <*> >>= >>
                             run run* alpha-num one-of* none-of* value
                             many many1 digit sep-by white-space bind
                             skip skip-ws return fwd sym* optional
                             new-line*]]
   [blancas.kern.lexer.basic :only [string-lit parens]]))

;;; Declarations

(declare scheme-expr)
(def scheme-expr (fwd scheme-expr))

;;;; A scheme reader

(defn- str-rest [s]
  (apply str (rest s)))

(defn scheme-read-string 
  "Same as core/read-string, but reads a scheme form"
  [string]
  (value scheme-expr string))
  
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

;;; Forward declare scheme-expr

;;; Symbols

(def special-char=
  (one-of* "!$%&*+-./:<=>?@^_~"))

(def symbol= (<+> (many1 (<|> alpha-num special-char=))))

(def symbol- (>>= symbol= #(return (symbol %))))

;;; Numbers

(def number= (<+> (optional (sym* \-))
                  (many1 digit)))

(def number- (>>= number= #(return (Integer/parseInt %))))

;;; Booleans

(def boolean= (>> (sym* \#)
                  (<|> (sym* \t) (sym* \f))))

(def boolean- (>>= boolean= #(return (= \t %))))

;;; Lists

(def list-elems= (many scheme-expr))

;; TODO: At the moment a dotted pair will just be a list
;;       with two elements, which will probably not preserve the
;;       semantics of cons, car and cdr.
(def dotted-pair-elms= (<*> scheme-expr
                            (>> (skip-ws (sym* \.))
                                scheme-expr)))

(def list= (parens (<|> (<:> dotted-pair-elms=)
                        list-elems=)))
                   
(def list-
  (>>= list=
       #(return (apply list %))))

;;; Comment

(def spaces (many white-space))

(def line-comment (<*> spaces
                       (sym* \;)
                       (many (none-of* "\n"))
                       new-line*
                       spaces))

(def scheme-ws (<|> (<:> line-comment)
                    spaces))
                    
;;; The parser

(def scheme-expr
  (>> scheme-ws (<|> list- number- symbol- string-lit boolean-)))

