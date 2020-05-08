#lang scheme
; Bilal Tekin-2017400264

;You can replace #f's with your function definitions and define more helper functions as you need to use this template.






; Solver function
(define TENTS-SOLUTION #f)



; ABS FUNCTION
(define (abs x)
  (
     if(< x 0) (- x)
       x
  )
  )

(define (makePair x y)
  (cons x (cons y null))
  )

; RETURN FIRST NOT FALSE ELEMENT WITH FUNCTIONS APPLIED
(define (myReturnFirstNotFalse fnc lst)
  (
   if(null? lst) #f
   (
    if(fnc (car lst)) (fnc (car lst))
     (myReturnFirstNotFalse fnc (cdr lst))
    )
   )
  
 )


; Check if x and y are neighbor or not
(define (myAdjacentFunction x y)
  (
   if (not (< (sqrt 2)(sqrt (+   (expt (- (car x) (car y)) 2) (expt (- (cadr x) (cadr y)) 2)   ))) ) #t
      #f
   )
 )

; Return a list ( () () () ()) which is the neighbors of the x
(define (myNeighborsListFunction x)
  (cons (cons (- (car x) 1 ) (cons (cadr x) null) )  
  (cons (cons (+ (car x) 1 ) (cons (cadr x) null) )
  (cons (cons (car x) (cons (- (cadr x) 1 ) null) )
  (cons (cons (car x) (cons (+ (cadr x) 1 ) null) ) null) ) ) )
 )

; Check x is adjacent with any element in the list
(define (myAdjacentWithListFunction x y)
  (if (null? y) #f
   (if (myAdjacentFunction x (car y)) #t
      (myAdjacentWithListFunction x (cdr y))
   )
  )

 )


; My Replace number in the list With N according to position
(define (myReplaceWithNFunction list position number)
    (cond [(= position 1) (cons number (cdr list))] 
          [else (cons (car list) (myReplaceWithNFunction (cdr list) (- position 1) number ))])

 )


; Helper functions
(define RETURN-FIRST-NOT-FALSE myReturnFirstNotFalse)

(define ADJACENT myAdjacentFunction)

(define ADJACENT-WITH-LIST myAdjacentWithListFunction )

(define NEIGHBOR-LIST myNeighborsListFunction)

(define REPLACE-NTH myReplaceWithNFunction)




