(##namespace ("oo#"

*self*
@
@!
%
class-of
<class>
<object>
send
define-class
))

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

(define-macro (oo#% obj msg . args)
  (define (send obj method-name . args)
    (parameterize ((*self* obj))
      ((oo#lookup-method (instance-class obj) msg) ,@args))))
