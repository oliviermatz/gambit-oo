(include "~~lib/oo/oo#.scm")

(define-class <point-2d>
  fields: (x: y:)

  methods:
  (init: ;; init: is called when a new object is created.
   (lambda (x y)
     (@! x: x) ;; *self* is a parameter that is dynamically bound
                    ;; to the current object.
     (@! y: y)) ;; $! sets a field value

   get-x:
   (lambda () (@ x:)) ;; $. gets a field value

   get-y:
   (lambda () (@ y:))))

(define-class <point-3d>
  parent: <point-2d> ;; only single inheritance is supported

  fields: (z:)

  methods:
  (init:
   (lambda (x y z)
     (super x y) ;; super is bound to the parent class method of the
                 ;; same name, or #f if there isn't any.
     (@! z: z))

   get-x: ;; methods can be overriden
   (lambda () (fl* (super) 10.))

   get-z:
   (lambda () (@ z:))))

;; % sends a message to an object.
;; Objects are created by sending the new: message to a class object.
;; Parameters are forwarded to the init: method.
(define p-2d (% <point-2d> new: 1. 2.))

(define p-3d (% <point-3d> new: 1. 2. 3.))

(display (send p-2d get-x:)) (newline)
(display (send p-2d get-y:)) (newline)

(display (send p-3d get-x:)) (newline)
(display (send p-3d get-y:)) (newline)
(display (send p-3d get-z:)) (newline)

(display p-2d)

;; (main)
