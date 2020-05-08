#lang scheme
; Bilal Tekin-2017400264

;You can replace #f's with your function definitions and define more helper functions as you need to use this template.

(require racket/trace)








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
(define (myReplaceWithNFunction lst position number)
    (cond [(= position 1) (cons number (cdr lst))] 
          [else (cons (car lst) (myReplaceWithNFunction (cdr lst) (- position 1) number ))])

 )

; Remove an element from a list ; (removeElementFromList '((1 2) (2 3)) '(2 3)) ; (removeElementFromList '(1 2 3) 1)
(define (removeElementFromList lst element)
  ( if (null? lst) lst
    (cond [(equal? (car lst) element)(cdr lst)] 
          [else (cons (car lst) (removeElementFromList (cdr lst) element ))])
   )

 )


; Create Table With x = row , y = row , lstSize = Table Size AxA, lst is the list Of Indices
(define (createTable x y lstSize lst)  
   (  
    if (and (equal? x 1) (equal? y 1)) (cons (cons x (cons y null)) lst)
       (
        
        if(equal? x 1) (cons (cons x (cons y null)) (createTable lstSize (- y 1) lstSize lst))
          (cons (cons x (cons y null)) (createTable (- x 1) y lstSize lst)) 
         )
    ) 
 )

(define (isElementInTheList x lst)
  (
   if(null? lst) #f
     (
      if(equal? x (car lst)) #t
      (isElementInTheList x (cdr lst))
      )
   )


 )



; Remove Unnecessary Cells in the Table
(define (removeUnnecessaryCells x lst filteredList)
  
  (
   
   if (null? x) x
  (
   ;((removeUnnecessaryCells   (cdr x)   lst filteredList))
   if (or (not (myAdjacentWithListFunction (car x) lst) ) (isElementInTheList (car x) lst))  (removeUnnecessaryCells   (cdr x)   lst filteredList) ; (removeElementFromList x (car x) )) ;
   (cons (car x) (removeUnnecessaryCells (cdr x) lst filteredList) )
      
   )
  
 )
  
  )
;(trace removeUnnecessaryCells)

; My Solver
(define (mySolution parameters)
  (define myTable (createTable (length (car parameters)) (length (cadr parameters)) (length (car parameters)) '()))
  (define onlyRequiredCells (removeUnnecessaryCells myTable (caddr parameters) '()))

  
  (newline)
  (display myTable)
  (newline)(newline)
  (display onlyRequiredCells)
  (newline)
  
 ) 

; Solver function
(define TENTS-SOLUTION mySolution)


;(define table '((1 2) (2 3) (3 4)))

;(define lastList (removeElementFromList table '(2 3)))


; Helper functions
(define RETURN-FIRST-NOT-FALSE myReturnFirstNotFalse)

(define ADJACENT myAdjacentFunction)

(define ADJACENT-WITH-LIST myAdjacentWithListFunction )

(define NEIGHBOR-LIST myNeighborsListFunction)

(define REPLACE-NTH myReplaceWithNFunction)




