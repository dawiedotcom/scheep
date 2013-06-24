(ns scheep.reader
  (:gen-class)
  (:use
   [scheep.primitives :only [make-pair]]
   [blancas.kern.core :only [<|> <:> <+> <*> >>= >> <<
                             alpha-num letter one-of* none-of* 
                             many many1 digit white-space end-by
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
  
;;; Spaces

(def spaces (many white-space))

;;; Symbols

(def special-initial=
  (one-of* "!$%&*/:<=>?^_~"))
(def special-subsequent=
  (one-of* "+-.@"))
(def initial= (<|> letter special-initial=))
(def subsequent= (<|> initial= digit special-subsequent=))

(def symbol= (<|> (<:> (<+> initial= (many1 subsequent=)))
                  initial=))

(def peculiar-symbol= (<|> (token* "...")
                           (token* "+")
                           (<< (token* "-")
                               (not-followed-by digit))))

(def ->symbol (>>= symbol= #(return (symbol (str %)))))

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
(def dotted-list-elems= (<*> (end-by spaces scheme-expr)
                             (>> (sym* \.)
                                 scheme-expr)))

(def list= (parens list-elems=))
(def dotted-list= (parens dotted-list-elems=))
                   
(def ->list
  (>>= list=
       #(return (apply list %))))

(def ->dotted-list
  (>>= dotted-list=
       #(make-pair return (apply list (first %)) (second %))))

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

(def line-comment (<*> (optional spaces)
                       (sym* \;)
                       (many (none-of* "\n"))
                       new-line*))

(def scheme-ws (<|> (<:> line-comment)
                    spaces))
                    
;;; Scheme expressions

(def scheme-expr
  (>> (skip-many scheme-ws)
      (<|> 
           (<:> ->peculiar-symbol)
           ->number
           ->symbol
           string-lit
           ->boolean
           ->quote
           ->quasiquote
           (<:> ->unquote-splicing)
           ->unquote
           (<:> ->list)
           ->dotted-list)))

(def scheme-expr+
  (many1 scheme-expr))
