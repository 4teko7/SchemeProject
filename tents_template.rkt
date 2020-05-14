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


; Sum Of Elements In the list
(define (sum elemList)
  (if
    (null? elemList)
    0
    (+ (car elemList) (sum (cdr elemList)))
  )
)

(define (makePair x y)
  (cons x (cons y null))
  )

; delete all occurences of an element from the list
(define (deleteAllOccurences item list) (filter (lambda (x) (not (equal? x item))) list))

; nth element of a list
(define (nth n l)
  (if (or (> n (length l)) (< n 0))
    (error "Index Out Of Bound Exception !")
    (if (eq? n 1)
      (car l)
      (nth (- n 1) (cdr l))))

  )


;(trace nth)

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


(define (removeDuplicatesListsInAList l)
  (cond ((null? l)
         '())
        ((member (car l) (cdr l))
         (removeDuplicatesListsInAList (cdr l)))
        (else
         (cons (car l) (removeDuplicatesListsInAList (cdr l))))))


;(trace removeDuplicates)


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
(define (myNeighborsListFunctionForSolution x absLst xCor yCor rows columns)
  (define x1  (if (or (< (- (car x) 1 ) 1)  (> (car x) xCor) (< (car x) 1) ( > (cadr x) yCor) ( < (cadr x) 1) ) null (if (or (member (cons (- (car x) 1 ) (cons (cadr x) null) ) absLst )      (not (< 0 (nth (- (car x) 1 ) rows))) (not (< 0 (nth (cadr x)  columns))) ) null  (cons (- (car x) 1 ) (cons (cadr x) null) ) )   ) )
  (define x2  (if (or (> (+ (car x) 1 ) xCor) (< (car x) 1) (> (car x) xCor)  (> (cadr x) yCor) (< (cadr x) 1) ) null (if (or (member (cons (+ (car x) 1 ) (cons (cadr x) null) ) absLst )   (not (< 0 (nth (+ (car x) 1 ) rows))) (not (< 0 (nth (cadr x)  columns))) ) null  (cons (+ (car x) 1 ) (cons (cadr x) null) ) )   ))
  (define x3  (if (or (< (- (cadr x) 1 ) 1) (> (car x) xCor) (< (car x) 1) (> (cadr x) yCor) (< (cadr x) 1)  )  null (if (or (member (cons (car x) (cons (- (cadr x) 1 ) null) ) absLst )     (not (< 0 (nth (- (cadr x) 1 ) columns))) (not (< 0 (nth (car x)  rows))) ) null (cons (car x) (cons (- (cadr x) 1 ) null) ) )  ))
  (define x4  (if (or (> (+ (cadr x) 1 ) yCor) (> (car x) xCor) (< (car x) 1) (> (cadr x) yCor) (< (cadr x) 1)  ) null (if (or (member (cons (car x) (cons (+ (cadr x) 1 ) null) ) absLst )  (not (< 0 (nth (+ (cadr x) 1 ) columns))) (not (< 0 (nth (car x)  rows))) ) null  (cons (car x) (cons (+ (cadr x) 1 ) null) ) )  ))
  (define lst null)
  (define lst1 (if (not (null? x1)) (cons x1 lst) lst))
  (define lst2 (if (not (null? x2)) (cons x2 lst1) lst1))
  (define lst3 (if (not (null? x3)) (cons x3 lst2) lst2))
  (define lst4 (if (not (null? x4)) (cons x4 lst3) lst3))
 (if (null? lst4) '() lst4)
 )

;(trace myNeighborsListFunctionForSolution)

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



; calculate individual lists for removing invalid combinations for edge numbers 2 
(define (calculateAndCheckForInvalidCombinationForEdgeNumbers lst xCorsAndyCors)
  (define x (if (null? lst) xCorsAndyCors (calculateAndCheckForInvalidCombinationForEdgeNumbers (cdr lst) xCorsAndyCors)))
  (if (not x) #f
  (if (null? lst) x   (if  (and (< 0 (nth (car (car lst)) (car x))) (< 0 (nth (cadr (car lst)) (cadr x))) )  (cons (myReplaceWithNFunction (car x) (car (car lst)) (- (nth (car (car lst)) (car x)) 1)) (cons (myReplaceWithNFunction (cadr x) (cadr (car lst)) (- (nth (cadr (car lst)) (cadr x)) 1)) null) )  #f ) )                                    
  )


)

; Check for edge numbers and remove invalid combinations for tents 1
(define (removeInvalidCombinationsForEdgeNumbers lst xCorsAndyCors)

  (define x (if (null? lst) null (removeInvalidCombinationsForEdgeNumbers (cdr lst) xCorsAndyCors)))
  (if (and (not (null? lst)) (calculateAndCheckForInvalidCombinationForEdgeNumbers (car lst) xCorsAndyCors)) (cons (car lst) x) x)
  )




;(trace removeUnnecessaryCells)

;(define (calculateAndCheckForInvalidCombinationForEdgeNumbers lst xCors yCors)



;)

;(define (removeInvalidCombinationsForEdgeNumbers lst xCors yCors)

 ; (define x (if (null? lst) null (removeUnvalidCombinationsForEdgeNumbers (cdr lst) xCors yCors)))
 ; (cons (calculateAndCheckForInvalidCombinationForEdgeNumbers (car lst) xCors yCors) x)


;  )




; '((1 1) (2 2)) + '(((1 1) (1 1)) ((2 2) (1 1)) ((1 1) (2 2)) ((2 2) (2 2))) = > List which has every sublist length of 3
(define (createTableNested mylst1 mylst2 absoluteList lst)
  (define x (length mylst1))
  (define y (length mylst2))
   (  
       cond [(and (equal? x 1) (equal? y 1) (not (member (car mylst1) (car mylst2))) (not (myAdjacentWithListFunction (car mylst1) (car mylst2) ) ) ) (cons (cons (car mylst1) (car mylst2)) lst)]
            [(and (equal? x 1) (equal? y 1) ) null]

            [else (
        
        cond [(and (equal? x 1) (not (member (car mylst1) (car mylst2))) (not (myAdjacentWithListFunction (car mylst1) (car mylst2) ) ))  (cons (cons (car mylst1) (car mylst2)) (createTableNested absoluteList (cdr mylst2) absoluteList lst))]
             [(equal? x 1) (createTableNested absoluteList (cdr mylst2) absoluteList lst)]
             [(and (not (member (car mylst1) (car mylst2))) (not (myAdjacentWithListFunction (car mylst1) (car mylst2) ) ) ) (cons (cons (car mylst1) (car mylst2)) (createTableNested (cdr mylst1) mylst2 absoluteList lst))]
             [else  (createTableNested (cdr mylst1) mylst2 absoluteList lst)]

             
         )]
            
    ) 
 )
;(trace createTableNested)
 

;'((1 1) (2 2)) + '((1 1) (2 2)) = > '(((1 1) (1 1)) ((2 2) (1 1)) ((1 1) (2 2)) ((2 2) (2 2)))
; Create Table With x = row , y = row , lstSize = Table Size AxA, lst is the list Of Indices
(define (createTableOfList mylst1 mylst2 absoluteList lst)
  (define x (length mylst1))
  (define y (length mylst2))
   (  
       cond [(and (equal? x 1) (equal? y 1) (not (equal? (car mylst1) (car mylst2))) (not (myAdjacentFunction (car mylst1) (car mylst2) ) ) ) (cons (cons (car mylst1) (cons (car mylst2) null)) lst)]
            [(and (equal? x 1) (equal? y 1) ) null]
       [else (
        
        cond [(and (equal? x 1) (not (equal? (car mylst1) (car mylst2))) (not (myAdjacentFunction (car mylst1) (car mylst2) ) ))  (cons (cons (car mylst1) (cons (car mylst2) null)) (createTableOfList absoluteList (cdr mylst2) absoluteList lst))]
             [(equal? x 1) (createTableOfList absoluteList (cdr mylst2) absoluteList lst)]
             [( and (not (equal? (car mylst1) (car mylst2))) (not (myAdjacentFunction (car mylst1) (car mylst2) ) ) ) (cons (cons (car mylst1) (cons (car mylst2) null)) (createTableOfList (cdr mylst1) mylst2 absoluteList lst))]
             [else  (createTableOfList (cdr mylst1) mylst2 absoluteList lst)]
         )]
    ) 
 )
;(trace createTableOfList)




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




(define (sendEveryElementListOfList lst absLst xCor yCor rows columns)
  (define x ( if (< 2 (length lst)) (sendEveryElementListOfList  (cdr lst) absLst xCor yCor rows columns) ( if (equal? 2 (length lst)) (cons (myNeighborsListFunctionForSolution (cadr lst) absLst xCor yCor rows columns) null) null ) ) )
  (cons (myNeighborsListFunctionForSolution (car lst) absLst xCor yCor rows columns)  x)
  )

;Send Every List inside a list to combination function
(define (sendEveryElementListOfListToCombinationFunction lst xCorsAndyCors)
  (define x ( if (< 2 (length lst)) (sendEveryElementListOfListToCombinationFunction  (cdr lst) xCorsAndyCors) ( if (equal? 2 (length lst)) (car (cons (createTableOfList (car lst) (cadr lst) (car lst) '()) null) ) null ) ) )
  
  (if (and (not (equal? 2 (length lst))) (not (equal? x null))) (removeInvalidCombinationsForEdgeNumbers (createTableNested  (car lst) x (car lst) '()) xCorsAndyCors)  x)
   
  )

;(trace sendEveryElementListOfListToCombinationFunction)
;(trace sendEveryElementListOfListToCombinationFunction)


(define (checkSolution1 parameters)
  
  (define xCorsAndyCors (cons (car parameters) (cons (cadr parameters) null)))
  (define onlyRequiredCells (sendEveryElementListOfList (caddr parameters) (caddr parameters) (length (car parameters)) (length (cadr parameters)) (car parameters) (cadr parameters)))    ;This gives Only Neighbors of the tree :  ((1 1) (2 2))  = >  '(   ((1 2) (2 1))   ((2 3) (2 1) (3 2) (1 2))   )
  
  (define filteredonlyRequiredCells (if (equal? (length onlyRequiredCells) 0) #f (deleteAllOccurences null onlyRequiredCells)))
  (define mathedTable (if (and (not (equal? filteredonlyRequiredCells #f)) (< 0 (length filteredonlyRequiredCells))  ) (if (equal? (length filteredonlyRequiredCells) 1) filteredonlyRequiredCells (sendEveryElementListOfListToCombinationFunction filteredonlyRequiredCells xCorsAndyCors) ) #f))

  ;( if (and (not (equal? filteredonlyRequiredCells #f)) (not (equal? mathedTable #f)) (> (length mathedTable) 0) )  (car (removeInvalidCombinationsForEdgeNumbers mathedTable xCorsAndyCors)) #f )
  ( if (and (not (equal? filteredonlyRequiredCells #f)) (not (equal? mathedTable #f)) (> (length mathedTable) 0) (equal? (length (car mathedTable)) (length (caddr parameters))) )  (car mathedTable) #f )
 
  )


; My Solver
(define (mySolution parameters)

  (cond
    [(and (null? (caddr parameters) ) (equal? (sum (car parameters)) 0)   (equal? (sum (cadr parameters)) 0) ) '()]
    [(and (null? (caddr parameters) ) (or (not (equal? (sum (car parameters)) 0))   (not (equal? (sum (cadr parameters)) 0)) ) ) #f]
    [(or (not  (equal? (sum (car parameters)) (sum (cadr parameters)))) (not (equal? (sum (cadr parameters)) (length (caddr parameters)))))  #f]
    [(not (equal? (length (removeDuplicatesListsInAList (caddr parameters))) (length (caddr parameters)) )) #f]
    [else (checkSolution1 parameters)]
    )

  )

;(trace removeInvalidCombinationsForEdgeNumbers)
;(trace sendEveryElementListOfListToCombinationFunction)
;(mySolution '((0 1 0 0) (0 1 0) ((2 3)) ))

; Solver function

(define TENTS-SOLUTION mySolution)


; Helper functions
(define RETURN-FIRST-NOT-FALSE myReturnFirstNotFalse)

(define ADJACENT myAdjacentFunction)

(define ADJACENT-WITH-LIST myAdjacentWithListFunction )

(define NEIGHBOR-LIST myNeighborsListFunction)

(define REPLACE-NTH myReplaceWithNFunction)


;(TENTS-SOLUTION '((2 0 1 0 1 1 1 1) (1 1 0 1 1 1 1 1) ((7 5) (8 6)) ))
;(TENTS-SOLUTION '((2 0 1 0 1 1 1 1) (1 1 0 1 1 1 1 1) ((1 6) (2 1) (2 4) (3 1) (4 5) (4 7) (5 5) (5 7) (6 2) (6 3) (7 5) (8 6) (8 8) (1 1) (2 2)) ))



  ;(display " lst And Length : ")(newline)(newline)
  ;(display (length lst))
  ;(display (car lst))(newline)(newline)
  ;(display " x : ")
  ;(display x)(newline)(newline)
