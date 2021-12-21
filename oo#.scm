(##namespace ("oo#"

*self*
$.
$!
@
class-of
<class>
<object>
send
get-field
set-field!

define-class

oo-header
oo-footer

))

(define-macro (define-macro! sig . body)
  (define (prefix? prefix s)
    (and (<= (string-length prefix) (string-length s))
	 (string=?
	  (substring s 0 (string-length prefix))
	  prefix)))

  (let ((symbols (make-table test: eq?)))
    (let loop ((exp body))
      (cond
       ((and (symbol? exp) (prefix? "g!" (symbol->string exp)))
	(table-set! symbols exp #t))
       ((list? exp) (for-each loop exp))
       (else #!void)))

    `(define-macro ,sig
       (let ,(map (lambda (s)
		    `(,s (string->symbol (string-append "oo#" (symbol->string (gensym ',s))))))
		  (map car (table->list symbols)))
	 ,@body))))

(define-macro (oo#oo-header)
  (eval
   '(begin
      (define oo#call-site-caches (list))
      (define oo#allow-inline-cache #t))))

(define-macro (oo#oo-footer)
  (if (pair? oo#call-site-caches)
    `(begin
       ,@(let loop ((acc '()) (call-site-caches oo#call-site-caches))
           (if (pair? call-site-caches)
             (loop (append `((define ,(caar call-site-caches) #f)
                             (define ,(cdar call-site-caches) #f))
                           acc)
                   (cdr call-site-caches))
             acc))
       (define oo#allow-inline-cache #f))
    #!void))

(define-macro! (oo#$. obj field-name)
  (unless (keyword? field-name)
    (raise (string-append "$. must be given a keyword, got"
                          (call-with-output-string
                           (lambda (p) (display field-name p))))))
  (set! oo#call-site-caches (cons (cons g!call-site-cache-class
                                        g!call-site-cache-field-offset)
                                  oo#call-site-caches))
  (if oo#allow-inline-cache
    `(let ((,g!obj ,obj))
       (let ((,g!class (oo#instance-class ,g!obj)))
         (if (not (eq? ,g!class ,g!call-site-cache-class))
           (let ((,g!field-offset (oo#get-field-offset ,g!class ,field-name)))
             (set! ,g!call-site-cache-field-offset ,g!field-offset)
             (set! ,g!call-site-cache-class ,g!class)))
         (##vector-ref (oo#instance-fields ,g!obj)
                       ,g!call-site-cache-field-offset)))
    `(oo#get-field ,obj ,field-name)))

(define-macro! (oo#$! obj field-name field-value)
  (unless (keyword? field-name)
    (raise (string-append "$! must be given a keyword, got"
                          (call-with-output-string
                           (lambda (p) (display field-name p))))))
  (set! oo#call-site-caches (cons (cons g!call-site-cache-class
                                        g!call-site-cache-field-offset)
                                  oo#call-site-caches))
  (if oo#allow-inline-cache
    `(let ((,g!obj ,obj))
       (let ((,g!class (oo#instance-class ,g!obj)))
         (if (not (eq? ,g!class ,g!call-site-cache-class))
           (let ((,g!field-offset (oo#get-field-offset ,g!class ,field-name)))
             (set! ,g!call-site-cache-field-offset ,g!field-offset)
             (set! ,g!call-site-cache-class ,g!class)))
         (##vector-set! (oo#instance-fields ,g!obj)
                        ,g!call-site-cache-field-offset
                        ,field-value)))
    `(oo#set-field! ,obj ,field-name ,field-value)))

(define-macro! (oo#@ obj method-name . args)
  (unless (keyword? method-name)
    (raise (string-append "@ must be given a keyword, got"
                          (call-with-output-string
                           (lambda (p) (display method-name p))))))
  (set! oo#call-site-caches (cons (cons g!call-site-cache-class
                                        g!call-site-cache-fn)
                                  oo#call-site-caches))
  (if oo#allow-inline-cache
    `(let ((,g!obj ,obj))
       (parameterize ((oo#*self* ,g!obj))
         (let ((,g!class (oo#instance-class ,g!obj)))
           (if (not (eq? ,g!class ,g!call-site-cache-class))
             (let ((,g!method (oo#lookup-method ,g!class ,method-name)))
               (set! ,g!call-site-cache-fn ,g!method)
               (set! ,g!call-site-cache-class ,g!class)))
           (,g!call-site-cache-fn ,@args))))
    `(oo#send ,obj ,method-name ,@args)))

(define-macro (oo#define-class name
			       #!key
			       (parent 'oo#<object>)
			       (methods '())
			       (fields '()))
  `(define ,name
     (oo#send oo#<class>
	      new:
	      ,(symbol->string name)
	      ,parent
	      (list
	       ,@(let loop ((acc '()) (methods methods))
		   (if (pair? methods)
		     (loop (cons `(cons ,(car methods)
				        (let ((super
					       (oo#lookup-method ,parent
								 ,(car methods))))
					  ,(cadr methods)))
				 acc)
			   (cddr methods))
		     (reverse acc))))
	      (list ,@fields))))
