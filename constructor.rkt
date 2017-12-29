#lang racket
#| Assignment 2 - Q3

This file contains Question 3. The class constructor
|#
(provide class-init)

; helper macro to help evaluate a list's contents
(define-syntax eval-lst
  (syntax-rules ()
    ; Evaluate list
    [(eval-lst '(<expr> ...))
     (list (eval-lst <expr>) ...)]
    ; Evaluate list element
    [(eval-lst <expr>)
     <expr>]))
#|
(class-init <Class> ([<arg> = <val>] ...)
                    [(instance <var> <eval>) ...]
                    [(self <attr> <expr>) ...]
                    [(<method> <param> ...) <body>] ...)

[<arg> = <val>] ... : These set of parameters are used as the parameters for the init function.
                      <arg> is the variable name for the parameter and <val> is the default value

(instance <var> <eval>) ... : These set of parameters are used as instance variables for the auxilary
                              computation. <var> is the variable name and <eval> is the auxilary value/computation.

(self <attr> <expr>) ... : These set of parameters are used as the class variables. <attr> is the variable
                           name and <expr> is the variable's value when the init function is used.

(<method> <param> ...) <body>] ... : Method for the class. Same as the given class macro.

All parameters are optional. If one set of parameters are not used then use and empty pair of brackets

Once all the parameters are gathered, the macro produces a <Class> object. If a user does not specify
variable values when creating an instance of the object, the constructor will use the default values
specified from macro. Local bindings are made first to the instance variables and then class attributes.
If an instance variable or class attribute requires another instance variable/ attribute then it must be declared
after the required variable. eval-lst is used to evaluate the contents of lists. Once the object is created the user
can request attributes and methods. If there is a naming conflict between an attribute and a method then the attribute
will be used first.
                              
|#


(define-syntax class-init
  (syntax-rules ()
    [(class-init <Class> ([<arg> = <val>] ...) ; init parameters
                 [(instance <var> <eval>) ...] ; instance variables for constructor
                 [(self <attr> <expr>) ...]    ; class attribute variables
                 [(<method> <param> ...) <body>] ...) ; methods
      (define (<Class> [<arg> <val>] ...)
        (let* ([<var> (eval-lst <eval>)] ...)
          (let* ([<attr> (eval-lst <expr>)] ...)
            (lambda (msg)
              (cond [(equal? msg (id->string <attr>)) <attr>]
                    ...
                    [(equal? msg (id->string <method>))
                     (lambda (<param> ...) <body>)]
                    ...
                    [else "Unrecognized message!"])))))]))

(define-syntax id->string
  (syntax-rules ()
    [(id->string <id>)
     (symbol->string (quote <id>))]))

; Example exrpressions
; Uncomment if you want to see output
#|
(define (f r)
  (+ r 5))

(class-init MyClass ([a = 0] [b = 0])
            ([instance r (f a)])
            ([self x (f a)]
             [self y '(b 100 r)]
             [self z "you are cool"]))

(let ([obj (MyClass 4 5)])
        (obj "x")) ; 9

(let ([obj (MyClass)])
        (obj "x")) ; 5

(let ([obj (MyClass 4 6)])
        (obj "y")) ; '(6 100 9)

(let ([obj (MyClass 4 6)])
        (obj "z")) ; "you are cool"

(let ([obj1 (MyClass 4 6)])
  (let ([obj2 (MyClass 1 2)])
        (list (obj1 "x") (obj1 "y") (obj2 "x") (obj2 "y")))) ; '(9 (6 100 9) 6 (2 100 6))
|#