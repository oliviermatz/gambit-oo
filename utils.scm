(define (index-of list elem)
  (let loop ((list list) (i 0))
    (cond
     ((null? list) #f)
     ((equal? (car list) elem) i)
     (else (loop (cdr list) (+ i 1))))))

(define (stringify . args)
  (call-with-output-string
   (lambda (p)
     (let loop ((args args))
       (if (pair? args)
           (display (car args) p))))))
