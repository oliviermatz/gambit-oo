(##namespace ("oo#"

*self*
$.
$!
@
class-of
<class>
<object>
define-class

))

(define-macro (oo#define-class name
			       #!key
			       (parent '<object>)
			       (methods '())
			       (fields '()))
  `(define ,name
     (oo#@ oo#<class>
	   new:
	   ,(symbol->string name)
	   ,parent
	   (list
	    ,@(let loop ((acc '()) (methods methods))
		(if (pair? methods)
		  (loop (append (list (car methods)
				      `(let ((super
					      (oo#lookup-method ,parent
								,(car methods))))
					 ,(cadr methods)))
				acc)
			(cddr methods))
		  acc)))
	   (list ,@fields))))
