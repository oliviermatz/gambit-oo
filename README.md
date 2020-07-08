# OO

A Smalltalk inspired, minimal implementation of an object system
for [Gambit Scheme](http://www.gambitscheme.org/wiki/index.php/Main_Page).

examples.scm

```scheme
(include "~~lib/oo/oo#.scm")
(load "~~lib/oo/oo.o1")

(define-class <point-2d>
  fields:
  (x: y:)

  methods:
  (init: ;; init: is called when a new object is created.
   (lambda (x y)
     ($! (*self*) x: x) ;; *self* is a parameter that is dynamically bound
                        ;; to the current object.
     ($! (*self*) y: y)) ;; $! sets a field value

   get-x:
   (lambda () ($. (*self*) x:)) ;; $. gets a field value

   get-y:
   (lambda () ($. (*self*) y:))))

;; @ sends a message to an object.
;; Objects are created by sending the new: message to a class object.
;; Parameters are forwarded to the init: method.
(define p-2d (@ <point-2d> new: 1. 2.))

(display (@ p-2d get-x:))
(display (@ p-2d get-y:))

(define-class <point-3d>
  parent: <point-2d> ;; only single inheritance is supported

  fields:
  (z:)

  methods:
  (init:
   (lambda (x y z)
     (super x y) ;; super is bound to the parent class method of the
                 ;; same name, or #f if there isn't any.
     ($! (*self*) z: z))

   get-x: ;; methods can be overriden
   (lambda () (fl* (super) 10.))

   get-z:
   (lambda () ($. (*self*) z:))))

(define p-3d (@ <point-3d> new: 1. 2. 3.))

(display (@ p-3d get-x:))
(display (@ p-3d get-y:))
(display (@ p-3d get-z:))
```