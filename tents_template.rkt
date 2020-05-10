#lang scheme
; Bilal Tekin-2017400264

;You can replace #f's with your function definitions and define more helper functions as you need to use this template.

(require racket/trace)




;(((3 3) (2 3)) ((3 3) (1 3)) ((2 3) (1 3)) ((3 3) (4 2)) ((2 3) (4 2)) ((1 3) (4 2)) ((3 3) (3 2)) ((2 3) (3 2)) ((1 3) (3 2)) ((4 2) (3 2)) ((3 3) (1 2)) ((2 3) (1 2)) ((1 3) (1 2)) ((4 2) (1 2)) ((3 2) (1 2)) ((3 3) (4 1)) ((2 3) (4 1)) ((1 3) (4 1)) ((4 2) (4 1)) ((3 2) (4 1)) ((1 2) (4 1)) ((3 3) (2 1)) ((2 3) (2 1)) ((1 3) (2 1)) ((4 2) (2 1)) ((3 2) (2 1)) ((1 2) (2 1)) ((4 1) (2 1)) ((3 3) (1 1)) ((2 3) (1 1)) ((1 3) (1 1)) ((4 2) (1 1)) ((3 2) (1 1)) ((1 2) (1 1)) ((4 1) (1 1)) ((2 1) (1 1)))
;(((3 3) (2 3)) ((3 3) (1 3)) ((2 3) (1 3)) ((3 3) (4 2)) ((2 3) (4 2)) ((1 3) (4 2)) ((3 3) (3 2)) ((2 3) (3 2)) ((1 3) (3 2)) ((4 2) (3 2)) ((3 3) (1 2)) ((2 3) (1 2)) ((1 3) (1 2)) ((4 2) (1 2)) ((3 2) (1 2)) ((3 3) (4 1)) ((2 3) (4 1)) ((1 3) (4 1)) ((4 2) (4 1)) ((3 2) (4 1)) ((1 2) (4 1)) ((3 3) (2 1)) ((2 3) (2 1)) ((1 3) (2 1)) ((4 2) (2 1)) ((3 2) (2 1)) ((1 2) (2 1)) ((4 1) (2 1)) ((3 3) (1 1)) ((2 3) (1 1)) ((1 3) (1 1)) ((4 2) (1 1)) ((3 2) (1 1)) ((1 2) (1 1)) ((4 1) (1 1)) ((2 1) (1 1)))
;(((3 3) (2 3)) ((3 3) (1 3)) ((2 3) (1 3)) ((3 3) (4 2)) ((2 3) (4 2)) ((1 3) (4 2)) ((3 3) (3 2)) ((2 3) (3 2)) ((1 3) (3 2)) ((4 2) (3 2)) ((3 3) (1 2)) ((2 3) (1 2)) ((1 3) (1 2)) ((4 2) (1 2)) ((3 2) (1 2)) ((3 3) (4 1)) ((2 3) (4 1)) ((1 3) (4 1)) ((4 2) (4 1)) ((3 2) (4 1)) ((1 2) (4 1)) ((3 3) (2 1)) ((2 3) (2 1)) ((1 3) (2 1)) ((4 2) (2 1)) ((3 2) (2 1)) ((1 2) (2 1)) ((4 1) (2 1)) ((3 3) (1 1)) ((2 3) (1 1)) ((1 3) (1 1)) ((4 2) (1 1)) ((3 2) (1 1)) ((1 2) (1 1)) ((4 1) (1 1)) ((2 1) (1 1)))

;(((3 2) (1 2)) ((3 2) (2 1)) ((1 2) (2 1)) ((3 2) (1 1)) ((1 2) (1 1)) ((2 1) (1 1)))
;(((3 2) (1 2)) ((3 2) (2 1)) ((1 2) (2 1)) ((3 2) (1 1)) ((1 2) (1 1)) ((2 1) (1 1)))
;(((3 2) (1 2)) ((3 2) (2 1)) ((1 2) (2 1)) ((3 2) (1 1)) ((1 2) (1 1)) ((2 1) (1 1)))
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


; Check If list a is in list b
(define (isRemainsSubset a b)
  (and ( not (null? b)) ( or (and (equal? (car (car a)) (car (car b))) (equal? (cadr (car a)) (cadr (car b)))) (isRemainsSubset a (cdr b ) )   )) 
  )

;Second Function Of isSetsEqual
(define (isSubset a b bOriginal)
  (or (null? a) 
      (and ( not (null? b)) ( or (and (equal? (car (car a)) (car (car b))) (equal? (cadr (car a)) (cadr (car b)))) (isRemainsSubset a (cdr b ))   ) 
           (isSubset (cdr a) bOriginal bOriginal )))

  )


;Check If Two Lists Are Equal In Terms Of Element, ignore order
(define (isSetsEqual a b)
  (if(null? b ) #f
                                   
  (if (isSetsEqual a (cdr b) ) #t
     (and (isSubset a (car b) (car b))
       ;(isSubset (car b) a a)
       )
  )
  )
)

;(trace isSetsEqual)

; Remove Duplicate Elements
(define (removeDuplicates l)
  (cond ((null? l)
         '())
        ((isSetsEqual (car l) (cdr l))
         (removeDuplicates (cdr l)))
        (else
         (cons (car l) (removeDuplicates (cdr l))))))

(trace removeDuplicates)
(removeDuplicates '( ((2 1) (3 2)) ((3 2) (2 1)) ) )
;(removeDuplicates '(((1 2) (2 1)) ((1 1) (2 1))))
;(removeDuplicates '(((1 2) (3 2)) ((2 1) (3 2)) ((1 1) (3 2)) ((3 2) (1 2)) ((1 1) (1 2)) ((3 2) (2 1)) ((1 2) (2 1)) ((1 1) (2 1)) ((3 2) (1 1)) ((1 2) (1 1)) ((2 1) (1 1))))


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


;Return list of all neighbors which are not null
(define (myNeighborsListFunctionForSolution x)
  (define x1  (if (< (- (car x) 1 ) 1) null (cons (- (car x) 1 ) (cons (cadr x) null) ) )   )
  (define x2  (if (> (+ (car x) 1 ) 4) null (cons (+ (car x) 1 ) (cons (cadr x) null) ) )   )
  (define x3  (if (< (- (cadr x) 1 ) 1) null (cons (car x) (cons (- (cadr x) 1 ) null) ) )  )
  (define x4  (if (> (+ (cadr x) 1 ) 4) null (cons (car x) (cons (+ (cadr x) 1 ) null) ) )  )
  (define lst null)
  (define lst1 (if (not (null? x1)) (cons x1 lst) lst))
  (define lst2 (if (not (null? x2)) (cons x2 lst1) lst1))
  (define lst3 (if (not (null? x3)) (cons x3 lst2) lst2))
  (define lst4 (if (not (null? x4)) (cons x4 lst3) lst3))
 (if (null? lst4) '() lst4)
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






; '((1 1) (2 2)) + '(((1 1) (1 1)) ((2 2) (1 1)) ((1 1) (2 2)) ((2 2) (2 2))) = > List which has every sublist length of 3
(define (createTableNested mylst1 mylst2 absoluteList lst)
  (define x (length mylst1))
  (define y (length mylst2))
   (  
       cond [(and (equal? x 1) (equal? y 1) (not (member (car mylst1) (car mylst2)))) (cons (cons (car mylst1) (car mylst2)) lst)]
            [(and (equal? x 1) (equal? y 1) ) null]

            [else (
        
        cond [(and (equal? x 1) (not (member (car mylst1) (car mylst2))))  (cons (cons (car mylst1) (car mylst2)) (createTableNested absoluteList (cdr mylst2) absoluteList lst))]
             [(equal? x 1) (createTableNested absoluteList (cdr mylst2) absoluteList lst)]
             [(not (member (car mylst1) (car mylst2))) (cons (cons (car mylst1) (car mylst2)) (createTableNested (cdr mylst1) mylst2 absoluteList lst))]
             [else  (createTableNested (cdr mylst1) mylst2 absoluteList lst)]

             
         )]
            
    ) 
 )


;'((1 1) (2 2)) + '((1 1) (2 2)) = > '(((1 1) (1 1)) ((2 2) (1 1)) ((1 1) (2 2)) ((2 2) (2 2)))
; Create Table With x = row , y = row , lstSize = Table Size AxA, lst is the list Of Indices
(define (createTableOfList mylst1 mylst2 absoluteList lst)
  (define x (length mylst1))
  (define y (length mylst2))
   (  
       cond [(and (equal? x 1) (equal? y 1) (not (equal? (car mylst1) (car mylst2)))) (cons (cons (car mylst1) (cons (car mylst2) null)) lst)]
            [(and (equal? x 1) (equal? y 1) ) null]
       [else (
        
        cond [(and (equal? x 1) (not (equal? (car mylst1) (car mylst2))))  (cons (cons (car mylst1) (cons (car mylst2) null)) (createTableOfList absoluteList (cdr mylst2) absoluteList lst))]
             [(equal? x 1) (createTableOfList absoluteList (cdr mylst2) absoluteList lst)]
             [(not (equal? (car mylst1) (car mylst2))) (cons (cons (car mylst1) (cons (car mylst2) null)) (createTableOfList (cdr mylst1) mylst2 absoluteList lst))]
             [else  (createTableOfList (cdr mylst1) mylst2 absoluteList lst)]
         )]
    ) 
 )


;(nestedList 2 '((1 1) (2 2)) '((1 1) (2 2)) 2)
;'(((1 1) (1 1) (1 1)) ((2 2) (1 1) (1 1)) ((1 1) (2 2) (1 1)) ((2 2) (2 2) (1 1)) ((1 1) (1 1) (2 2)) ((2 2) (1 1) (2 2)) ((1 1) (2 2) (2 2)) ((2 2) (2 2) (2 2)))
; Finds all Permutation of the list. 
(define (nestedList n lst)
  (cond [(equal? n 0) null]
        [(equal? n 1) lst]
        [(> n (length lst)) null]
        [(equal? n 2) (removeDuplicates (createTableOfList lst  lst lst '()))]
        [else (removeDuplicates (createTableNested lst (nestedList (- n 1) lst) lst '()) )  ]
   )

  )

;(trace nestedList)




(define (sendEveryElementListOfList lst)
  
  
     
  (define x ( if (< 2 (length lst)) (sendEveryElementListOfList  (cdr lst)) ( if (equal? 2 (length lst)) (cons (myNeighborsListFunctionForSolution (cadr lst) ) null) null ) ) )
  (cons (myNeighborsListFunctionForSolution (car lst) )  x)


  )





;(define (isCellAvailable x))

; My Solver
(define (mySolution parameters)

  ;(define myTable (createTable (length (car parameters)) (length (cadr parameters)) (length (car parameters)) '()))
  (define onlyRequiredCells (sendEveryElementListOfList (caddr parameters)))
  
  ;(define numberOfTrees (length (caddr parameters)))
  ;(define allCombinations (nestedList numberOfTrees onlyRequiredCells) )
  (define numberOfOnlyRequiredCells (length onlyRequiredCells))

  ;(newline)
  ;(display allCombinations)
  ;(newline)
  ;(display myTable)
  (newline)(newline)
  (display onlyRequiredCells)
  
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




