(ns scheep.reader
  (:gen-class)
  (:use
   [clojure.core :exclude [read-string]]
   [blancas.kern.core :only [<|> <:> <+> <*> >>= >> <<
                             alpha-num one-of* none-of* 
                             many many1 digit white-space 
                             skip-many skip-ws return fwd sym* optional
                             new-line* print-error parse token* 
                             not-followed-by parse-file]]
   [blancas.kern.lexer.basic :only [string-lit parens]]))

;;; Declarations

(declare scheme-expr
         scheme-expr+
         parse-scheme)
(def scheme-expr (fwd scheme-expr))
(def scheme-expr+ (fwd scheme-expr+))

;;;; A scheme reader

(defn scheme-read-string 
  "Same as core/read-string, but reads a scheme form"
  [string]
  (parse-scheme parse
                scheme-expr
                string))
  
(defn scheme-read-string*
  "Like scheme-read-string, but reads more than one form"
  [string]
  (parse-scheme parse
                scheme-expr+
                string))
  
(defn scheme-read-file 
  "Reads a list of scheme forms from the given input file"
  [filename]
  (parse-scheme parse-file
                scheme-expr+
                filename))

(defn parse-scheme [parse-fn p to-parse]
  (let [parsed (parse-fn p to-parse)]
    (if-not (:error parsed)
      (:value parsed)
      (print-error parsed))))
  
;;; Symbols

(def special-char=
  (one-of* "!$%&*+-./:<=>?@^_~"))

(def symbol= (<+> (many1 (<|> alpha-num special-char=))))

(def peculiar-symbol= (<|> (token* "...")
                           (token* "+")
                           (<< (token* "-")
                               (not-followed-by digit))))

(def ->symbol (>>= symbol= #(return (symbol %))))

(def ->peculiar-symbol (>>= peculiar-symbol= #(return (symbol %))))

;;; Numbers

(def number= (<+> (optional (sym* \-))
                  (many1 digit)))

(def ->number (>>= number= #(return (Integer/parseInt %))))

;;; Booleans

(def boolean= (>> (sym* \#)
                  (<|> (sym* \t) (sym* \f))))

(def ->boolean (>>= boolean= #(return (= \t %))))

;;; Lists

(def list-elems= (many scheme-expr))

;; TODO: At the moment a dotted pair will just be a list
;;       with two elements, which will probably not preserve the
;;       semantics of cons, car and cdr.
(def dotted-pair-elms= (<*> scheme-expr
                            (>> (skip-ws (sym* \.))
                                white-space
                                scheme-expr)))

(def list= (parens (<|> (<:> dotted-pair-elms=)
                        list-elems=)))
                   
(def ->list
  (>>= list=
       #(return (apply list %))))

;;; Quote, quasiquote, unquote and unquote-splicing

(def quote= (>> (sym* \') scheme-expr))
(def quasiquote= (>> (sym* \`) scheme-expr))
(def unquote= (>> (sym* \,) scheme-expr))
(def unquote-splicing= (>> (sym* \,) (sym* \@) scheme-expr))

(defn bind-symbol-in-head [symbol p]
  (>>= p #(return (list symbol %))))

(def ->quote (bind-symbol-in-head 'quote quote=))
(def ->quasiquote (bind-symbol-in-head 'quasiquote quasiquote=))
(def ->unquote (bind-symbol-in-head 'unquote unquote=))
(def ->unquote-splicing (bind-symbol-in-head
                        'unquote-splicing
                        unquote-splicing=))

;;; Comment

(def spaces (many white-space))

(def line-comment (<*> (optional spaces)
                       (sym* \;)
                       (many (none-of* "\n"))
                       new-line*))

(def scheme-ws (<|> (<:> line-comment)
                    spaces))
                    
;;; Scheme expressions

(def scheme-expr
  (>> (skip-many scheme-ws)
      (<|> ->list
           (<:> ->peculiar-symbol)
           ->number
           ->symbol
           string-lit
           ->boolean
           ->quote
           ->quasiquote
           (<:> ->unquote-splicing)
           ->unquote)))

(def scheme-expr+
  (many1 scheme-expr))
