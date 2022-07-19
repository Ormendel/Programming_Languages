#lang pl 02
;Submitted by Or Mendel, 315524389


#|-------------------------------------------------------|#

;;;==========Question 1 :String Expressions, Writing BNF for SE==========

#|
1.a
Let's write below the possible types SE can be:

<SE> ::=  <CHARS> | <NUMBER> | <STRING>
 
<DIGIT> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 ;;; only one of these 10 options

<CHARS> ::= #\<DIGIT> | #\<DIGIT> <CHARS>    e.g #\8 #\2 #\0
          
;;NUMBER can be digit, or digit and number, or representing size of a string (by using string-length function)
<NUMBER> ::= <DIGIT> | <DIGIT> <NUMBER> | {string-length <STRING>}


<STRING> ::=
         <NUMBER>
         | {string-append <STRING> <STRING>} e.g ( string-append ( string #\3 #\3 #\5 ) "80" )
         | {string-append <STRING> <CHARS>} e.g  (string-append "45"  #\8)
         | {string-append <STRING>}  e.g  (string-append "222331")
         | {string-insert <STRING> <CHARS> <NUMBER>} e.g ( string-insert "2227" #\4 66 )
         | {number->string <NUMBER>}     e.g  number->string(string-length("12345"))

          Referring to empty string is quite simple 
         | {string-append lambda lambda}
         | {string <CHARS>}  ;create a string from a group of chars

valid expressions:
                  "12344"
                  12
                  ( string #\1 #\2 #\4 )
                  ( string-append ( string #\1 #\2 #\4 ) "12" )
                  ( string-insert "1357" #\4 66 )
                  ( number->string 156879 )
                  ( number->string ( string-length "0033344" ) )
                  ( string-append "45" ( number->string ( string-length "0033344" ) ))
                  ( string-append )
                  ( string-append "" ( string-insert "1357" #\4 66 ) "" )
                  #\3

Invalid expressions:
                  "a2b"
                  12 13 4 67
                  ( string 124 )
                  ( string-append ( string-length "44" ) "12" )
                  ( string-insert "1357" 4 66 )
                  ( number->string "156879" )
                  ( string-append 33 44 66)
                  #\3 #\4
                  #\32
                  #\q
|#

#|
1.b

1)
<SE>
 |
 V
<STRING>
 |
 V
{string-append "123" {string-append "447" "8"} }
 |                                   |      |
 V                                   V      V
 "123"                             "567"    <CHARS>
 |                                   |      |
 V                                   V      V
<CHARS>                             <CHARS>  #\8
 |                                   |
 V                                   V
#\1 #\2 #\3                      #\4 #\4 #\7
                           
2)
<SE>
  |
  V
<STRING>
  |
  V
{string-length "123"}
  |
  V
 <NUMBER>
  |
  V
 <DIGIT>
  |
  V
  3        

3)
<SE>
 |
 V
<STRING>
 |
 V
{number->string  144442}
 |
 V
String
|
V
"144442"

|#

#|-------------------------------------------------------|#
;;;==========Question 2 : Higher Order Functions==========

#|
Use foldl together with (or without) map to define a sum-of-squares function
which takes a list of numbers as input, and produces a number which is the sum
of the squares of all of the numbers in the list.
e.g: (test (sum-of-squares '(1 2 3)) => 14)|#

#|fold1 needs to know the procedure which works on each item in the list.
Therefore, we need to declare function that get a number and return the square of it|#
(: square : Number -> Number)
#|
@description: This function gets a number (positive/negative) and return the square of it - simply by multiply num*num
@input: num of type Number
@output: (num*num) of type Number
|#
(define (square num)
  (* num num))
;; tests
(test (square 2.5) => 6.25)
(test (square 0) => 0)
(test (square -5) => 25)
(test (square -1001.3) => 1002601.69)

;Now for our main function sum-of-squares
(: sum-of-squares : (Listof Number) -> Number);//This function gets input list of numbers, and return sum of each item square of type Number.
(define (sum-of-squares list)
  (foldl + 0 (map square list)));While square is our procedure for fold1
;Base case - sum is zero when list is empty, or else we add it to the square function on each value in our list
#|fold1 + this expression based fold1 definition|#

;; tests
(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '()) => 0)
(test (sum-of-squares '(1 1 1 1 1 1)) => 6)
(test (sum-of-squares '(4 -2 22 0)) => 504)
(test (sum-of-squares '(-1 -1 -1)) => 3)
(test (sum-of-squares '(-2 0 0)) => 4)


#|-------------------------------------------------------|#
;;;==========Question 3 : PAE (and more H.O. functions)==============================
;This source helped me for this question: https://github.com/xGreenbean/RacketScience/blob/master/314095605_1.rkt
;;;==========Question 3 .a. - createPolynomial==============================

;@ input list of numbers (the coefficients of the polinom)
;@ output a function from number to number, input is x value to eval the polynom at given x0.
;; The output is an accumalator (polyX)
(: createPolynomial : (Listof Number) -> (Number -> Number))
(define (createPolynomial coeffs)
  (: poly : (Listof Number) Number Integer Number -> Number)
  (define (poly args x power acc)
    ;; if our list is empty we return the accumelator (which has the value of the polynomial) - stopping condition
    (if (null? args)
        acc
        ;; else call poly with rest of the list same x value to evaluate and cacualate the current value and add to accum.
        (poly (rest args) x (+ power 1) (+ acc (* (expt x power) (first args)))))
    )
  (: polyX : Number -> Number)
  (define (polyX x)
    ;;call poly with the list of coeffs, given x, and initialize power to be 0 and accumulator to be 0
    (poly coeffs x 0 0));tail function call
  ;;return polyx
  polyX);PolyX call from createPolynomial

;;;Tests 3.a
(define p1226 (createPolynomial '(1 2 2 6)))
(test (p1226 0) =>
      (+ (* 1 (expt 0 0)) (* 2 (expt 0 1)) (* 2 (expt 0 2)) (* 6
                                                               (expt 0 3))))
(test (p1226 4) =>
      (+ (* 1 (expt 4 0)) (* 2 (expt 4 1)) (* 2 (expt 4 2)) (* 6
                                                               (expt 4 3))))
(test (p1226 11) => (+ (* 1 (expt 11 0)) (* 2 (expt 11 1)) (* 2
                                                              (expt 11 2)) (* 6 (expt 11 3))))
(define p591 (createPolynomial '(5 9 1)))
(test (p591 11) => (+ (* 5 (expt 11 0)) (* 9 (expt 11 1)) (* 1
                                                             (expt 11 2))))
(define p_0 (createPolynomial '()))
(test (p_0 4) => 0)
(test (p_0 12220) => 0)
(test (p_0 1031) => 0)
(test (p_0 12) => 0)

;;;==========Question 3 .b. - PLANG==============================

#| b.i

 The grammar:
 <PLANG> ::= {{poly <AEs> }{<AEs> }}
 <AEs> ::= <AE> | <AE> <AEs>
 <AE> ::= same as described in class
 |#

; b.ii Parser for PLANG
(define-type PLANG
  [Poly (Listof AE) (Listof AE)])

(define-type AE
  [Num Number]
  [Add AE AE]
  [Sub AE AE]
  [Mul AE AE]
  [Div AE AE])

;We can get either String or Sexpr
;If we get Sexpr the returned type will be AE
;If we get String the returned type will be PLANG
(: parse-sexpr : Sexpr -> AE)
;; converting s-expressions into AEs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n) (Num n)]
    [(list '+ left_ex right_ex)
     (Add (parse-sexpr left_ex) (parse-sexpr right_ex))]
    [(list '- left_ex right_ex)
     (Sub (parse-sexpr left_ex) (parse-sexpr right_ex))]
    [(list '* left_ex right_ex)
     (Mul (parse-sexpr left_ex) (parse-sexpr right_ex))]
    [(list '/ left_ex right_ex)
     (Div (parse-sexpr left_ex) (parse-sexpr right_ex))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> PLANG)
;; parses a string containing a PLANG expression to a PLANG AST
(define (parse str)
  (let ([body (string->sexpr str)])
    (match body
      ;; if pattern is poly followed by empty list we throw error
      [(list (cons 'poly '()) (list rest ...)) (error 'parse "at least one coefficient is required in ~s" body)]
      ;; if pattern is ((poly (non empty list)) (empty list)) we throw error
      [(list (cons 'poly first) '()) (error 'parse "at least one point is required in ~s" body)]
      ;;otherwise we assume current syntax and use map to parse each of the list "AEs", which is the correct format
      [(list (cons 'poly first) (list rest ...)) (Poly (map parse-sexpr first) (map parse-sexpr rest))]
      [else (error 'parse "bad syntax in ~s" body)])))

;;;tests 3.bii
(test (parse "{{poly 1 2 3} {1 2 3}}")
      => (Poly (list (Num 1) (Num 2) (Num 3))
               (list (Num 1) (Num 2) (Num 3))))
#|(test (parse "{{poly } {1 2} }") 
 =error> "parse: at least one coefficient is
 required in ((poly) (1 2))")
(test (parse "{{poly 1 2} {} }")
 =error> "parse: at least one point is
 required in ((poly 1 2) ())")|#
(test (parse "{{polyt 1 2} {} }")
      =error> "parse: bad syntax in ((polyt 1 2) ())")

;b.iii - evaluation process
;; evaluates AE expressions to numbers
(: eval : AE ->  Number )
(define (eval expr)
  (cases expr
    [(Num n) n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (/ (eval l) (eval r))]))

(: eval-poly : PLANG -> (Listof Number) );; evaluates PLANG expressions to list of numbers
(define (eval-poly p-expr)
  (cases  p-expr
    ;; if we have Poly type of variant
    ;; we create polynomial from evaluating all "AES" in the first list
    ;; then we use the returned function to caculate value at point (which are parsed from AEs with map and eval)
   
    [(Poly coeffs points) (map (createPolynomial (map eval coeffs)) (map eval points))]))

(: run : String -> (Listof Number))
;; evaluate a FLANG program contained in a string
(define (run str)
  (eval-poly (parse str)))


(test (run "{{poly 1 2 3} {1 2 3}}")
      => '(6 17 34))
(test (run "{{poly 4 2 7} {1 4 9}}")
      => '(13 124 589))
(test (run "{{poly 1 2 3} {1 2 3}}")
      => '(6 17 34))
(test (run "{{poly 4/5 } {1/2 2/3 3}}")
      => '(4/5 4/5 4/5))
(test (run "{{poly 2 3} {4}}")
      => '(14))
(test (run "{{poly 1 1 0} {-1 3 3}}")
      => '(0 4 4))