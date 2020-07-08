(##namespace ("oo#"))
(##include "~~lib/gambit#.scm")

(define-type instance
  class
  fields)

(define *self*
  (make-parameter #f))

(define (keyword-list->offset-table lst)
  (list->table
   (let loop ((acc (list)) (lst lst) (i 0))
     (if (pair? lst)
       (loop (cons (cons (car lst) i) acc)
	     (cdr lst)
	     (fx+ i 1))
       acc))
   size: (length lst)
   test: eq?
   hash: eq?-hash))

(define (flat-assoc-list->table lst)
  (let loop ((t (make-table size: (fxquotient (length lst) 2)
			    test: eq?
			    hash: eq?-hash))
	     (lst lst))
    (if (pair? lst)
      (begin
	(table-set! t (car lst) (cadr lst))
	(loop t (cddr lst)))
      t)))

(define field-names-of-class
  '(name:
    parent:
    local-methods:
    methods-table:
    local-field-names:
    field-offsets-table:
    field-count:))

(define field-offsets-table-of-class
  (keyword-list->offset-table field-names-of-class))

(define (get-field-offset obj field-name)
  (let* ((class (instance-class obj))
	 (field-offsets-table (if (eq? class <class>)
				field-offsets-table-of-class
				($. class field-offsets-table:))))
    (table-ref field-offsets-table field-name #f)))

(define ($. obj field-name)
  (vector-ref (instance-fields obj)
	      (get-field-offset obj field-name)))

(define ($! obj field-name field-value)
  (vector-set! (instance-fields obj)
	       (get-field-offset obj field-name)
	       field-value))

(define (lookup-method class method-name)
  (and class
       (table-ref ($. class methods-table:)
		  method-name
		  #f)))

(define (@ obj method-name . args)
  (parameterize ((*self* obj))
    (apply (lookup-method (instance-class obj) method-name)
	   args)))

(define (class-of obj)
  (instance-class obj))

(define (wr-object we obj)
  (##wr-sn we obj 'object ($. (class-of obj) name:)))

(define methods-of-class
  (list

   new:
   (lambda args
     (let* ((self (*self*))
	    (instance (make-instance self (make-vector ($. self field-count:) #f)))
	    (method (lookup-method self init:)))
       (if method
	 (parameterize ((*self* instance))
	   (apply method args)))
       instance))

   init:
   (lambda (name parent local-methods local-field-names)
     (let* ((self (*self*))
	    (field-names
	     (let loop ((class parent) (acc local-field-names))
	       (if class
		 (loop ($. class parent:)
		       (append acc ($. class local-field-names:)))
		 acc))))
       ($! self name: name)
       ($! self parent: parent)
       ($! self local-methods: local-methods)
       ($! self
	   methods-table:
	   (flat-assoc-list->table
	    (let loop ((class parent) (acc local-methods))
	      (if class
		(loop ($. class parent:)
		      (append ($. class local-methods:) acc))
		acc))))
       ($! self local-field-names: local-field-names)
       ($! self field-offsets-table: (keyword-list->offset-table field-names))
       ($! self field-count: (length field-names))))

   wr:
   (lambda (we)
     (wr-object we (*self*)))))

(define <class>
  (make-instance
   #f
   (vector
    "<class>"
    #f
    methods-of-class
    (flat-assoc-list->table methods-of-class)
    field-names-of-class
    field-offsets-table-of-class
    (length field-names-of-class))))

(instance-class-set! <class> <class>)

(let* ((old-wr ##wr)
       (new-wr
	(lambda (we obj)
	  (if (instance? obj)
	    (@ obj wr: we)
	    (old-wr we obj)))))
  (set! ##wr new-wr))

(define <object>
  (@ <class>
     new:
     "<object>"
     #f
     (list wr:
	   (lambda (we)
	     (wr-object we (*self*))))
     (list)))
