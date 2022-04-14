(##namespace ("oo#"))
(##include "~~lib/gambit#.scm")
(##include "oo#.scm")

(##include "utils.scm")
(##include "fast-lookup.scm")

(define-type instance
  class
  fields)

(define *self*
  (make-parameter #f))

(define fields-of-class
  '(name:
    parent:
    local-methods:
    local-field-names:
    method-lookup-fn:
    field-offset-lookup-fn:
    field-count:))

(define field-offset-lookup-fn-offset
  (index-of fields-of-class field-offset-lookup-fn:))

(define (get-field-offset class field-name)
  (let ((lookup-fn (##vector-ref (instance-fields class) field-offset-lookup-fn-offset)))
    (lookup-fn field-name)))

(define (raise-no-such-field obj field-name)
  (raise (stringify "No field " field-name " in object " obj "\n")))

(define (get-field obj field-name)
  (let* ((offset (get-field-offset (instance-class obj) field-name)))
    (unless offset (raise-no-such-field obj field-name))
    (##vector-ref (instance-fields obj) offset)))

(define (@ field-name)
  (get-field (*self*) field-name))

(define (set-field! obj field-name field-value)
  (let* ((offset (get-field-offset (instance-class obj) field-name)))
    (unless offset (raise-no-such-field obj field-name))
    (##vector-set! (instance-fields obj) offset field-value)))

(define (@! field-name field-value)
  (set-field! (*self*) field-name field-value))

(define method-lookup-fn-offset
  (index-of fields-of-class method-lookup-fn:))

(define (lookup-method class method-name)
  (let ((lookup-fn (##vector-ref (instance-fields class) method-lookup-fn-offset)))
    (lookup-fn method-name)))

(define (send obj method-name . args)
  (parameterize ((*self* obj))
    (apply (lookup-method (instance-class obj) method-name) args)))

(define (class-of obj)
  (instance-class obj))

(define (wr-object we obj)
  (##wr-sn we obj 'object (get-field (class-of obj) name:)))

(define field-count-offset
  (index-of fields-of-class field-count:))

(define (make-offset-mapping keys)
  (let loop ((acc '()) (keys keys) (i 0))
    (if (pair? keys)
        (loop (cons (cons (car keys) i) acc)
              (cdr keys)
              (fx+ i 1))
        (reverse acc))))

(define (concat-from-parents acc field-name)
  (let loop ((class parent) (acc acc))
    (if class
      (loop (get-field class parent:)
	    (append (get-field class field-name) acc))
      acc)))

(define <class>
  (let* (;; the new method creates a new object of class (*self*)
         (new (lambda args
                (let* ((self (*self*))
                       (field-count (##vector-ref (instance-fields self) field-count-offset))
                       (fields (make-vector field-count #f))
	               (instance (make-instance self fields))
	               (init-method (lookup-method self init:)))
                  ;; This init method has nothing to do with the init method below, it's
                  ;; the one that was overriden by a user defined class.
                  (if init-method
                    (parameterize ((*self* instance))
	              (apply init-method args)))
                  instance)))
         ;; the init method initializes a newly created class object
         (init (lambda (name parent local-methods local-field-names)
                 (let* ((self (*self*))
	                (field-names (concat-from-parents local-field-names local-field-names:)))
                   (set-field! self name: name)
                   (set-field! self parent: parent)
                   (set-field! self local-methods: local-methods)
                   (set-field! self method-lookup-fn:
	                       (make-lookup-fn (concat-from-parents local-methods local-methods:)))
                   (set-field! self local-field-names: local-field-names)
                   (set-field! self field-offset-lookup-fn:
                               (make-lookup-fn (make-offset-mapping field-names)))
                   (set-field! self field-count: (length field-names)))))
         (methods-of-class
          `((new: . ,new)
            (init: . ,init)
            (wr: . ,(lambda (we) (wr-object we (*self*)))))))
   (make-instance
    #f
    (vector
     ;; name:
     "<class>"
     ;; parent:
     #f
     ;; local-methods:
     methods-of-class
     ;; local-field-names:
     fields-of-class
     ;; method-lookup-fn:
     (make-lookup-fn methods-of-class)
     ;; field-offset-lookup-fn:
     (make-lookup-fn (make-offset-mapping fields-of-class))
     ;; field-count:
     (length fields-of-class)))))

(instance-class-set! <class> <class>)

(define <object>
  (send <class>
        new:
        ;; name:
        "<object>"
        ;; parent:
        #f
        ;; local-methods:
        `((wr: . ,(lambda (we) (wr-object we (*self*)))))
        ;; local-field-names
        '()))

(let* ((old-wr ##wr)
       (new-wr
	(lambda (we obj)
	  (if (instance? obj)
	    (send obj wr: we)
	    (old-wr we obj)))))
  (set! ##wr new-wr))
