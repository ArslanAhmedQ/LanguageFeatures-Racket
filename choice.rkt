#| Choice Implementation

This is a cleaned-up version of the code from lecture.
While you are responsible for both understanding the
implementation and public API, only the latter is
required for this assignment.

We strongly recommend not changing this file.
|#
#lang racket
(provide -< next all clear ?-)

#|
(-< <expr> ...)
  Each <expr> is an arbitrary Racket expression.

  Evaluates and returns the first <expr>.
  If there is more than one argument, stores a choice point
  which resumes the program at where the (-< ...) is used,
  but with the remaining choices.
|#
(define-syntax -<
  (syntax-rules ()
    ; When there is only one option, return it.
    [(-< <expr1>) <expr1>]
    ; When there is more than one, return the first and store the rest.
    [(-< <expr1> <expr2> ...)
     (let/cc cont
       (begin  ;added here
       ; Push a new choice onto choices.
         (add-choice! (lambda () (cont (-< <expr2> ...))))
         (add-peek-choice! '(<expr2> ...))   ;added here  
          <expr1>)
       )]))


#|
(next)

  Backtracks to the most recently stored choice point and
  resume program execution from there, or returns "false."
  if there are no choice points stored.
> (-< 1 2 3)
1
> (next)
2
> (next)
3
> (next)
"false."
|#
(define (next)  ;no change has been done in this method 
  ; Check if there are any remaining choices
  (if (empty? choices)
      "false."
      ; Notice that it's ((get-choice!)) and not (get-choice!).
      ; What's the difference?
      ((get-choice!)))) ;the difference without extra parenthesie it just calls
                        ;the procedures, want to call the return value of get-choice!
                        ;so add extra parenthesis



#|
(all <expr>)
  <expr> is a Racket expression, possibly containing choice points.
  
  Returns a list of all possible outcomes of evaluating <expr>
  (i.e., making all possible combinations of the choices).
|#
(define-syntax all
  (syntax-rules ()
    [(all <expr>)
     (let* (
            ; The list to store all the choices. Because macros are hygienic,
            ; we don't get name conflicts if we use all multiple times.
            [all-results '()]
            ; A helper function which mutates all-results.
            [all-helper 
             (lambda (e)
               (set! all-results (cons e all-results))
               ; If (next) calls a continuation, the last expression
               ; (reverse choices) will not execute (and hence not be
               ; returned. But if (next) returns "false." then the last
               ; expression does get executed.
               (next)
               (reverse all-results))])
       (all-helper <expr>))]))

#|
(clear)

  Remove all choice points from stack. Used for testing purposes.
|#
(define (clear)
  (set! choices '()))


#|
(?- pred expr)
  pred: a unary predicate (i.e., boolean function)
  expr: a choice expression

  Returns a choice from 'expr' which satisfies 'pred'.
  Calling 'next' will return all possible choices
  that satisfy 'pred'.

  
|#
(define (?- pred expr)
  (if (pred expr)
      expr
      ; If the predicate fails, try the next choice.
      (next)))


;------------------------------------------------------------------------------
; Private values for managing the stack of choices.
;------------------------------------------------------------------------------

; The stack of choice points, represented as a list.
(define choices '())

; "Push": add a choice to the choices stack.
(define (add-choice! choice)
  (set! choices
        (cons choice choices)))

; "Pop": remove and return first choice from
; the choices stack.
(define (get-choice!)
  (let ([choice (first choices)])
    (set! choices (rest choices))
    (set! peek-choices (rest peek-choices)) ;added here 
    choice))

; Added these methods
; It's a stack of peek-choice points, represented as a list 
(define peek-choices '())

; push method: add a peek-choice to the peek-choices stack.
(define (add-peek-choice! choice)
  (set! peek-choices
        (cons choice peek-choices)))

;
(define (peek)
  (if (empty? choices)
      "false."
      (cons (quote -<)(first peek-choices))
      ;(cons (' -<)(first peek-choices))
      )
  )

#| Main Documents for changes
- Implemented a stack for the peek-choices, where the points are represented as a list
- Implemented a add-peek-choice! that's just work like add-choice! except it adds the
  choice to the stack peek-choices
- Modified the get-choice method so it also returns and remove the first choice from the peek-choices stack
  these two stacks are two different stack, they don't interfare with each other 

- Design of peek: Implemented a totally new method that checks the choices stack, if not empty just simply
  returns the cons of quote amb operator with top choice (first) on the peek-choices stack

- Nothing has been changed in next method, but one can also change the next method so it pops from the peek-choices
  stack then we dont have to modify the get0choice method 
- In the amb operator, pushed a new peek-choices on to peek-choices stack, so the amb operator also works with it.
  Also, begin block added so <expr> are evaluated in order, and the result of everything is accepted except the
  very last <exp
|#
  
