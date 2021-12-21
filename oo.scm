(##namespace ("oo#"))
(##include "~~lib/gambit#.scm")
(##include "oo#.scm")

(oo#oo-header)

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

(define-type instance
  class
  fields)

(define *self*
  (make-parameter #f))

(define method-lookup-fn-offset 3)
(define field-offset-lookup-fn-offset 5)
(define field-count-offset 6)

(define field-offsets-of-class
  `((name: . 0)
    (parent: . 1)
    (local-methods: . 2)
    (method-lookup-fn: . ,method-lookup-fn-offset)
    (local-field-names: . 4)
    (field-offset-lookup-fn: . ,field-offset-lookup-fn-offset)
    (field-count: . ,field-count-offset)))

(define class-field-offset-lookup-fn
  (make-lookup-fn field-offsets-of-class))

(define (get-field-offset class field-name)
  ((if (##eq? class <class>)
     class-field-offset-lookup-fn
     (##vector-ref (instance-fields class) field-offset-lookup-fn-offset))
   field-name))

(define (raise-no-such-field obj field-name)
  (raise (call-with-output-string
          (lambda (p)
            (display "No field " p)
            (display field-name p)
            (display " in object " p)
            (display obj p)
            (newline p)))))

(define (get-field obj field-name)
  (let ((offset (get-field-offset (instance-class obj) field-name)))
    (unless offset (raise-no-such-field obj field-name))
    (##vector-ref (instance-fields obj)
	          offset)))

(define (set-field! obj field-name field-value)
  (let ((offset (get-field-offset (instance-class obj) field-name)))
    (unless offset (raise-no-such-field obj field-name))
    (##vector-set! (instance-fields obj)
	           (get-field-offset (instance-class obj) field-name)
	           field-value)))

(define (lookup-method class method-name)
  (if (##eq? class <class>)
    (class-method-lookup-fn method-name)
    (let ((lookup (##vector-ref (instance-fields class) method-lookup-fn-offset)))
      (lookup method-name))))

(define (send obj method-name . args)
  (parameterize ((*self* obj))
    (apply (lookup-method (instance-class obj) method-name) args)))

(define (class-of obj)
  (instance-class obj))

(define (wr-object we obj)
  (##wr-sn we obj 'object ($. (class-of obj) name:)))

(define (new-class . args)
  (let* ((self (*self*))
         (field-count (##vector-ref (instance-fields self) field-count-offset))
         (fields (make-vector field-count #f))
	 (instance (make-instance self fields))
	 (method (lookup-method self init:)))
    (if method
      (parameterize ((*self* instance))
	(apply method args)))
    instance))

(define (init-class name parent local-methods local-field-names)
  (let* ((self (*self*))
	 (field-names
	  (let loop ((class parent) (acc local-field-names))
	    (if class
	      (loop ($. class parent:)
		    (append ($. class local-field-names:) acc))
	      acc))))
    ;; (##repl-debug)
    ($! self name: name)
    ($! self parent: parent)
    ($! self local-methods: local-methods)
    ($! self method-lookup-fn:
	(make-lookup-fn
	 (let loop ((class parent) (acc local-methods))
	   (if class
	     (loop ($. class parent:)
		   (append ($. class local-methods:) acc))
	     acc))))
    ($! self local-field-names: local-field-names)
    ($! self field-offset-lookup-fn:
        (make-lookup-fn
         (let loop ((acc '()) (field-names field-names) (i 0))
           (if (pair? field-names)
             (loop (cons (cons (car field-names) i) acc)
                   (cdr field-names)
                   (fx+ i 1))
             (reverse acc)))))
    ($! self field-count: (length field-names))))

(define methods-of-class
  `((new: . ,new-class)
    (init: . ,init-class)
    (wr: . ,(lambda (we) (wr-object we (*self*))))))

(define class-method-lookup-fn
  (make-lookup-fn methods-of-class))

(let* ((old-wr ##wr)
       (new-wr
	(lambda (we obj)
	  (if (instance? obj)
	    (send obj wr: we)
	    (old-wr we obj)))))
  (set! ##wr new-wr))

(define <class>
  (make-instance
   #f
   (vector
    "<class>" ;; name:
    #f ;; parent:
    methods-of-class ;; local-methods
    class-method-lookup-fn ;; method-lookup-fn:
    field-offsets-of-class ;; local-field-names:
    class-field-offset-lookup-fn ;; field-offset-lookup-fn:
    (length field-offsets-of-class)) ;; field-count:
   ))

(instance-class-set! <class> <class>)

(define methods-of-object
  `((wr: . ,(lambda (we) (wr-object we (*self*))))))

(define <object>
  (send <class>
        new:
        "<object>" ;; name:
        #f ;; parent:
        methods-of-object ;; local-methods:
        '() ;; local-field-names
        ))

(oo#oo-footer)
