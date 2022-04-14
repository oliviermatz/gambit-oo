(define max-mask-bits 13)
(define max-switch-table-size (fxarithmetic-shift 1 max-mask-bits))

(define (shift-mask shift mask hash)
  (bitwise-and (fxarithmetic-shift-right hash shift) mask))

(define (distinct? . xs)
  (let ((t (make-table test: eqv?)))
    (let loop ((xs xs))
      (if (pair? xs)
	(if (table-ref t (car xs) #f)
	  #f
	  (begin
	    (table-set! t (car xs) #t)
	    (loop (cdr xs))))
	#t))))

(define (maybe-min-hash hashes)
  (let loop ((mask-bits 1))
    (if (fx<= mask-bits max-mask-bits)
      (let ((shift-and-mask
	     (let ((mask (fx- (fxarithmetic-shift 1 mask-bits) 1)))
	       (let loop ((shift 0))
		 (if (fx< shift 32)
		   (if (apply distinct?
			      (map (lambda (hash)
				     (shift-mask shift mask hash))
				   hashes))
		     (list shift mask)
		     (loop (fx+ shift 1)))
 		   #f)))))
	(or shift-and-mask
	    (loop (fx+ mask-bits 1))))
      #f)))

(define (make-lookup-fn key-value-pairs)
  (let* ((hashes (map (lambda (kvp)
                        (##symbol-hash (car kvp)))
                      key-value-pairs))
         (shift-and-mask (maybe-min-hash hashes)))
    (if shift-and-mask
      (let* ((shift (car shift-and-mask))
             (mask (cadr shift-and-mask))
             (table (make-vector (fx* 2 (fx+ mask 1)) #f)))
        (for-each (lambda (kvp)
                    (let ((i (fx* (shift-mask shift mask (##symbol-hash (car kvp))) 2)))
                      (vector-set! table i (car kvp))
	              (vector-set! table (fx+ i 1) (cdr kvp))))
                  key-value-pairs)
        (lambda (k)
          (let ((idx (##fxarithmetic-shift
		      (##bitwise-and
		       (##fxarithmetic-shift-right
			(##symbol-hash k)
			shift)
		       mask)
		      1)))
            (and (##fx< idx (vector-length table))
		 (##eq? (vector-ref table idx) k)
                 (vector-ref table (##fx+ idx 1))))))
      (let ((table (make-table test: eq?)))
        (let loop ((key-value-pairs key-value-pairs))
          (when (pair? key-value-pairs)
            (table-set! table (caar key-value-pairs) (cdar key-value-pairs))
            (loop (cdr key-value-pairs))))
        (lambda (k)
          (table-ref table k #f))))))
