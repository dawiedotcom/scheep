

(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...)
      val ...))))

(define-syntax or
  (syntax-rules ()
    ((_) false)
    ((_ test) test)
    ((_ test1 test2 ...)
     (let ((x test1))
       (if x x (or test2 ...))))))
