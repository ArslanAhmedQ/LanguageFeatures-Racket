#| Assignment 2 - Using Backtracking

This file contains starter code for questions 4-6,
which involve using backtracking in interesting ways, and
extending the functionality of the backtracking library.
|#
#lang racket

; Import choice API
(require "choice.rkt")

; Export functions for testing. Please don't change this line!
(provide subsets sudoku-4 fold-<)

; QUESTION 3
#|
(subsets lst)
  lst: a list

  A choice expression which yields a subset of 'lst'.
  Repeated calls to 'next' should yield *all* other subsets
  (i.e., 'next' returns "false." only when all subsets of 'lst'
  have been returned).

  The subsets can be yielded in any order; however, no subset
  can appear twice.

  Note that:
    - A subset isn't the same as a sublist. Items don't have to be consecutive.
    - A subset can be empty; the empty set is a subset of any list.
    - Order doesn't matter in subsets
  
  The following is an example of how 'subsets' is used.
  Note that your implementation might yield the subsets
  in a different order than the one shown here.

> (subsets '(1 2))
'()
> (next)
'(1)
> (next)
'(2)
> (next)
'(1 2)
> (next)
"false."
|#


(define (add-to-all lst item) ; lst is a list of lists
      (cond [(equal? (length lst) 0)
             (list (list item))]
            [(equal? (length lst) 1)
             (cons item (first lst))]
            [else (cons (cons item (first lst))
                        (add-to-all (rest lst) item ))]))
 
;(add-to-all '((1 2) (1) ()) 3)


;(require racket/trace)
;(trace-
(define (subsets lst)
  (if (equal? lst null)
      '()
      (let ((rst (subsets (rest lst))))
        (-< (remove-duplicates (flatten (add-to-all rst (first lst))))
            rst))))

#|
(define (subsets lst)
  (if (empty? lst) (list '())
      (let [(rest (subsets (rest lst)))]
        (-< (remove-duplicates (flatten (rest (map(lambda (x)
                             (cons (first lst) x))))))
                          rest)))) |#
        
;(subsets '( 1 2 ))

; QUESTION 4
#|
(sudoku-4 puzzle)
  puzzle: a nested list representing a 4-by-4 Sudoku puzzle

  A choice expression that represents possible solutions to the puzzle.
  Upon evaluation, just one solution is returned, but repeated calls
  to 'next' produces more possible solutions, if any.

  Hint: use the ?- function in your solution. Again, your main task
  is just to correctly express the constraints, and let the computer
  do the work.

(define (?- pred expr)
  (if (pred expr)
      expr
      ; If the predicate fails, try the next choice.
      (next)))
|#
(define (check-row row)
  (equal? '(1 2 3 4) (sort row < )))

(define (check-column lstFunc rows)
  (equal? '(1 2 3 4) (sort (list (lstFunc (first rows))
                                 (lstFunc (second rows))
                                 (lstFunc (third rows))
                                 (lstFunc (fourth rows)))
                           <)))

(define (check-columns rows)
  (and (check-column fourth rows)
       (check-column third rows)
       (check-column second rows)
       (check-column first rows)))

(define (first-quarter rows)
  (list (first (first rows)) (first (second rows)) 
        (second (first rows)) (second (second rows))))

(define (second-quarter rows)
  (list (first (third rows)) (first (fourth rows)) 
        (second (third rows)) (second (fourth rows))))

(define (third-quarter rows)
  (list (third (first rows)) (third (second rows)) 
        (fourth (first rows)) (fourth (second rows))))

(define (fourth-quarter rows)
  (list (third (third rows)) (third (fourth rows)) 
        (fourth (third rows)) (fourth (fourth rows))))

(define (check-quarter quarter)
  (equal? '(1 2 3 4) (sort quarter <)))

(define (check-quarters rows)
  (and (check-quarter (fourth-quarter rows))
       (check-quarter (third-quarter rows))
       (check-quarter (second-quarter rows))
       (check-quarter (first-quarter rows))))

(define (sudoku-4 puzzle)
     (?- sudoku-helper
         (map (lambda (lst)
                (?- check-row
                    (map(lambda (x)
                          (if (string? x)
                              (-< 1 2 3 4)
                              x)) ;make sure that 1 2 3 4 is written in as string 
                        lst)))
              puzzle)))

(define (check-rows-false rows)
  (cond [(equal? (first rows) "false.") #t]
        [(equal? (second rows) "false.") #t]
        [(equal? (third rows) "false.") #t]
        [(equal? (fourth rows) "false.") #t]
        [else #f]))
                               
                          
(define (sudoku-helper rows)
  (cond [(equal? rows "false.") #f]
      [(check-rows-false rows) #f]
      [else (and (check-columns rows)
                 (check-quarters rows))]))
                                                      
         

; QUESTION 5
#|
(fold-< combine init expr)
  combine: a binary function
  init: an initial value
  expr: a choice expression

  Evaluate all choices in <expr> and combine them, one at a time, with the
  initial value, and return the result.

  Note that the order of <combine>'s parameters is the same as foldl:
    1) The value of the next choice
    2) The value of <init>
|#
;(require racket/trace)
;(trace-
(define-syntax fold-<
  (syntax-rules ()
    [(fold-< <combine> <init> <exp>)
     (foldl <combine> <init> (all <exp>))]  ;use of all from chocie
    ))

;(fold-< max 0 (+ (* (-< 1 2 3 4) (+ (-< 100 200) (-< 1 2)))))
;(foldr <combine> <init> (all <exp>))]