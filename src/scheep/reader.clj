(ns scheep.reader
  (:gen-class))

;;;; A scheme reader

(defn- str-rest [s]
  (apply str (rest s)))

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
