#lang pl
;Submitted by Or Mendel, 315524389


;;;==========Question 1==========

#|1.1 The function append5 – consumes 5 characters and returns the concatenation of them. |#


(: append5 : Char Char Char Char Char -> String);Declaration of the function append5

;Implementation
(define (append5 c1 c2 c3 c4 c5)
  (string c1 c2 c3 c4 c5))

;;;==========Tests 1.1==========

(test (append5 #\a #\b #\c #\d #\e) => "abcde") ;;;Pass
;(test (append5 #\b 1 #\a #\a #\a) = error> 'append5 "The function needs to get 5 characters") ;;;Error



#|1.2 The function permute3 – consumes 3 characters and returns a list of strings concatenation of them. |#


(: permute3 : Char Char Char -> (Listof String));Declaration of the function permute3

;Implementation
(define (permute3 c1 c2 c3)
  (list (string c1 c2 c3) (string c1 c3 c2) (string c2 c1 c3) (string c2 c3 c1) (string c3 c1 c2) (string c3 c2 c1)))


;;;==========Tests 1.2==========

(test (permute3 #\a #\b #\c) => '("abc" "acb" "bac" "bca" "cab" "cba")) ;;;Pass
;(test (permute3 #\x) = error> 'permute3 "The function needs to get 3 characters") ;;;Error
;(test (permute3 5) = error> 'permute3 "got a number, expected 3 characters") ;;;Error


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;==========Question 2==========

#|           2.1
Define a recursive function named count-3lists that
consumes a list of lists (where the type of the elements in the inner
lists may be any type) and returns the number of inner lists (within the
wrapping list) that contain exactly 3 elements.|#

;Let's write helper function that calculates length of a list (learnt in practice)
(: list-length : (Listof Any) -> Natural);Declaration

;Implemenation
(define (list-length list)
  (if (null? list)
      0
      (+ 1 (list-length(rest list)))))


;Let's write helper function to count to check if a list has 3 members
;;;Source: https://stackoverflow.com/questions/37014301/defining-a-function-that-accepts-a-list-of-lists-in-racket

#|If the length of the list is 3 - we return 1, else return 0|#
(: count-3lists-helper : (Listof Any) -> Natural)
(define (count-3lists-helper l)
  (cond [(= 3(list-length l)) 1]
        [else 0]))

(: count-3lists : (Listof (Listof Any)) -> Natural);Declaration of the function count-3lists

;Implementation
(define (count-3lists mylist)
  (cond
    [(null? mylist) 0]
    [else (+ (count-3lists-helper (first mylist)) (count-3lists (rest mylist)))]))

;;;==========Tests 2.1==========

(test (count-3lists '((1 3 4) (() (1 2 3)) ("tt"
                                            "Three" 7) (2 4 6 8) (1 2 3))) => 3);Passed
(test (count-3lists '((5 #t #f 4) (() (1 2 3) 7) ("tt"
                                                  "Four" 7) (2 4 6 8) (1 2 3) (5 #\a 4))) => 4);Passed
(test (count-3lists '((5 6) (5 5 5) (5 1 1))) => 2);Passed
(test (count-3lists '((5 6) (5 2 5 2 2 2) (5 1 1 2 2 2))) => 0);Passed
;(test (count-3lists (2)) =error> count-3lists "didn't get list of lists! syntax error");Error in syntax




#|           2.2
Define a function named count-3lists-tail that
works the same as count-3lists, but now using tail-recursion|#

;Let's write helper function that calculates length of a list (learnt in practice) - tail recursive
(: list-length-tail : ( Listof Any ) -> Natural )
( define ( list-length-tail ls )
   (: helper-list-length-tail : Natural ( Listof Any ) -> Natural )
   ( define ( helper-list-length-tail acc ls )
      (if ( null? ls )
          acc
          ( helper-list-length-tail (+ 1 acc) ( rest ls ))))
   (helper-list-length-tail 0 ls));call to function with accumulator initialized to be ZERO

;Similar to 2.1 - let's use helper function to indicate if sub-list has exactly 3 members
(: count-3lists-helper-tail : (Listof Any) -> Natural)
(define (count-3lists-helper-tail l)
  (cond [(= 3(list-length-tail l)) 1]
        [else 0]))

(: count-3lists-tail : (Listof (Listof Any)) -> Natural);Declaration of the function count-3lists

;Implementation
(define (count-3lists-tail mylist)
  (cond
    [(null? mylist) 0]
    [else (+ (count-3lists-helper-tail (first mylist)) (count-3lists-tail (rest mylist)))]));The calculation is the same as in 2.1

;;;==========Tests 2.2==========

(test (count-3lists-tail '((1 3 4) (() (1 2 3)) ("tt"
                                                 "Three" 7) (2 4 6 8) (1 2 3))) => 3);Passed
(test (count-3lists-tail '((5 #t #f 4) (() (1 2 3) 7) ("tt"
                                                       "Four" 7) (2 4 6 8) (1 2 3) (5 #\a 4))) => 4);Passed
(test (count-3lists-tail '((5 6) (5 5 5) (5 1 1))) => 2);Passed
(test (count-3lists-tail '((5 6) (5 2 5 2 2 2) (5 1 1 2 2 2))) => 0);Passed
;(test (count-3lists-tail 2) =error> count-3lists "didn't get list of lists! syntax error");Error in syntax


#|           2.3
Write an additional function count-3listsRec that is similar to the
above count-3lists, however counts the number of lists of length 3
recursively (i.e., on all levels of nesting).
|#

(: count-3lists-nestedhelperlist : (Listof Any) Natural -> Natural);Declaration of function that calculates list-length 3 in all levels, using our main recursion
;Implementation
#|Note: Parameter 'a' is 1 if and only if the length of the list is 3, otherwise it will be zero.
Furthermore, if it is of length 3, we need to send another call with paramter 0, because we don't want to include accidently a "fake" 3 length list.
e.g. '(1 2 3 4) - '(2 3 4) should not be included!|#
(define (count-3lists-nestedhelperlist l a)
  (cond
    [(null? l) 0]
    [else (cond
            [(= 3 (list-length l)) (+ a (count-3lists-nestedhelperany (first l)) (count-3lists-nestedhelperlist (rest l) 0))]
            [else (+ (count-3lists-nestedhelperany (first l)) (count-3lists-nestedhelperlist (rest l) 0))]
            )]
    )
  )

(: count-3lists-nestedhelperany : Any -> Natural);Declaration of function that decides wheather the paramter is of type list or not
;Implemenation
(define (count-3lists-nestedhelperany x)
  (cond
    [(not (list? x)) 0]
    [else (count-3lists-nestedhelperlist x 1)]
    )
  )
    
(: count-3listsRec : (Listof (Listof Any)) -> Natural);Declaration of the function count-3listsRec

;Implementation
(define (count-3listsRec mylist)
  (if (null? mylist)
      0
      (+ (count-3lists-nestedhelperany (first mylist)) (count-3lists-nestedhelperlist (rest mylist) 0))))

;;;==========Tests 2.3==========
(test (count-3listsRec '( (1 3 4) ( () (1 2 3) ) ("tt" "Three" 7) (2 4 6 8) (1 2 3))) => 4)
(test (count-3listsRec '( (1 2) (5 6 8) (5 5 5)) )=> 2)
(test (count-3listsRec '( (() () (1 2 3)) ) )=> 2)
(test (count-3listsRec '( () () (1 2 3) ) )=> 1)
(test (count-3listsRec '( (1 2 3 4 )))=> 0)
(test (count-3listsRec '( (1 2 3 )))=> 1)
(test (count-3listsRec '( (() (1 2) (1 2 3) )))=> 2)
(test (count-3listsRec '( ((1 2 3 4) (1 2 3) (() (1 2) (1 2 3) )))) => 4)
(test (count-3listsRec '( (1 (1 2 3 4) (1 2 3) (() (1 2) (1 2 3) )))) => 3)
(test (count-3listsRec '( (0) () (1 2) ((1 2 3 4) (1 2 3) (() (1 2) (1 2 3) ))))=> 4)
(test (count-3listsRec '( (0) () (1 2) ("try1" #t #f)))=> 1)
(test (count-3listsRec '(((#\a #\b #\c)((#\d #\e #\f)(#\e #\f #\g)(#\a #\b #\c)))(#\a #\b)))=> 5)
(test (count-3listsRec '(( (#\a #\b #\c) ( (#\d #\e #\f) (#\e #\f #\g) (#\a #\b #\c) ) (#\a #\b) )))=> 6)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;==========Question 3==========


;;; Defenitions
(: search-stack  : (Symbol KeyStack -> (U String False)) );Either we return string or false
(: pop-stack : (KeyStack -> (U KeyStack False) ));Returned value can be KeyStack of false
#|           3.1
Implement the empty stack EmptyKS – this should be a variant of the
data type (constructor).|#

#|           3.2
Implement the push operation Push – this too should be a variant of the
data type. The push operation should take as input a symbol (key), a
string (value), and an existing keyed-stack and return an extended keystack in the natural way – see examples below.|#
(define-type KeyStack
  [EmptyKS];EmptyKS is a variant representing empty stack of type KeyStack
  [Push Symbol String KeyStack]);it's also a variant which is operation that extends a given KeyStack 


#|           3.3
The search-stack operation takes as input a symbol (key) and a keyed-stack and return the first
(LIFO, last in first out) value that is keyed accordingly.
If the key does not appear in the original stack, it should return a 'False' value
|#

(define (search-stack given_key given_stack)
  (cases given_stack
    [(EmptyKS) #f]
    [(Push curr_symbol curr_string rest_stack) (if (eq? given_key curr_symbol)
                                                   curr_string
                                                   (search-stack given_key rest_stack))];Extend KeyStack in search-stack function
    )
  )
#|           3.4
The pop-stack operation takes as input a keyed-stack and
return the keyed-stack without its first (keyed) value.
If the original stack was empty, it return a 'False' value
|#
(define (pop-stack given_stack)
  (cases given_stack
    [(EmptyKS) #f]
    [(Push curr_symbol curr_string rest_stack)
     rest_stack];In this function we only need to remove the first value, so it's enough to return rest of stack without doing nothing else more complicated.
    )
  )

;;;==========Tests 3==========
(test (EmptyKS) => (EmptyKS))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) =>
      (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))
      => (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a
                                                         "A" (EmptyKS))))) => "AAA")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a
                                                         "A" (EmptyKS))))) => #f)
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A"
                                                   (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (EmptyKS)) => #f)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;==========Question 4==========

(: is-odd? : Natural -> Boolean)
#|
@description: this method is responsible to determine if an input natural number
              is odd or not(=even)
@logic: the functions works recursively as follows:
        for the basic case step, if x is zero then x is even so the function returns false
        for the recursive step x will be odd if x-1 will be even using is-even? method
@input x which is Natural Number
@output true x is odd Natural Number
|#

;The function reduces the number by 1 until it's zero.
;If the final answer is true, that means the original number is even because it's the only way to get true, so we need to return false in our tests (simply by writing not).
(define (is-odd? x)
  (if (zero? x)
      false
      (is-even? (- x 1))))

(: is-even? : Natural -> Boolean)
#|This function gets a natural number as input
  and outputs boolean true if the number is even, false otherwise.
  The function reduces the number by 1 until it's zero.
  If the final answer is true, that means the original number is even because it's the only way to get true, so we need to return true.|#
(define (is-even? x)
  (if (zero? x)
      true
      (is-odd? (- x 1))))

;; tests --- is-odd?/is-even?
(test (not (is-odd? 12)))
(test (is-even? 12))
(test (not (is-odd? 0)))
(test (is-even? 0))
(test (is-odd? 1))
(test (not (is-even? 1)))

(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
#|
This is generic function that checks if every element of type A can be satisfying function of type (A -> Boolean)
@input: list of type A and pred of type A -> Boolean
@output: true if for every x in list, (pred x) -> true
|#
(define (every? pred lst)
  (or (null? lst)
      (and (pred (first lst)) (every? pred (rest lst)))))

;; An example for the usefulness of this polymorphic function
(: all-even? : (Listof Natural) -> Boolean)
#|
This method wraps the function every? with specific values, and call every? with
list of Natural numbers and pred function from (Natural -> Boolean) which checks if
on an input x, x is even.
@output: true if for every x in the given list, (pred x) -> true
|#
(define (all-even? lst)
  (every? is-even? lst))

;; tests
(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7))))
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6))))


(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) ->
                  Boolean))
#|
Generic function thats check for each index i in {1,2} if list_i satisfying pred_i
@input: list1 of type B, list2 of type A, pred2 of type (B -> Boolean) and pred1 of type (A -> Boolean)
@output: true if for each (x,y) in (list1,list2) then (pred1 x,pred2 y) returns true
|#

(define (every2? pred1 pred2 lst1 lst2)
  (or (null? lst1) ;; both lists assumed to be of same length
      (and (pred1 (first lst1))
           (pred2 (first lst2))
           (every2? pred1 pred2 (rest lst1) (rest lst2)))))




