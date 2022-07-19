#lang pl

#|

Assignment #3: Submitted by 206262123_315524389

==========================PART A======================================
-----------------------------------------------------
1. BNF

Pretty straight forward, no difficulties.
Time: ~10 minutes
-----------------------------------------------------
<SOL> :: = { <NumList> }
        |  { scalar-mult <num> <SOL> }
        |  { intersect <SOL> <SOL>}
        |  { union <SOL> <SOL> } 
        |  <id>
        |  { with {<id> <SOL> } <SOL> } ;; this should be a syntactic sugar
       
<NumList> :: =  λ | <NumList><num> ;; where λ stands for the empty word, i.e., { } is the empty set

;; where <num> is any expression identified by Racket as a Number
;; and <id> is any expression such that Racket identifies '<id> as a symbol
 
|#


;; -----------------------------------------------------
;; The abstract syntax tree SOL
(define-type SET = (Listof Number))
(define-type SOL
  [Set  SET]
  [Smult Number SOL]
  [Inter SOL SOL]
  [Union SOL SOL]
  [IdS    Symbol]
  [WithS  Symbol SOL SOL])

;; ----------------------------------------------------
;; 2.Parser

;; Work process: We watched and understood a similiar process that was done in a lecture and implemnted it for our needs
;; Difficulties: No major difficulties, just some syntax errors and small things
;; Time: ~1-2 hours
;;-----------------------------------------------------

(: parse-sexprS : Sexpr -> SOL)
;; to convert s-expressions into SOLs
(define (parse-sexprS sexpr)
  (match sexpr
    [(list (number: ns) ...) (Set ns)] 
    [(symbol: name) (IdS name)]
    [(cons 'with more);Instead of doing match more, we do here match sexpr, just like learnt in lecture.
     (match sexpr
       [(list 'with (list (symbol: name) named-expr) body)
        (WithS name (parse-sexprS named-expr) (parse-sexprS body))]
       [else (error 'parse-sexprS "bad `with' syntax in ~s" sexpr)])]
    [(list 'scalar-mult (number: sc) rhs) (Smult sc (parse-sexprS rhs))];The function is Smult for parser, set-smult for evaluation
    [(list 'intersect lhs rhs) (Inter (parse-sexprS lhs) (parse-sexprS rhs))]
    [(list 'union lhs rhs) (Union (parse-sexprS lhs) (parse-sexprS rhs))]
    [else (error 'parse-sexprS "bad syntax in ~s" sexpr)]))


(: parseS : String -> SOL)
;; parses a string containing a SOL expression to a SOL AST
(define (parseS str)
  (parse-sexprS (string->sexpr str)))

  
(test (parseS "{1 3 4 1 4 4 2 3 4 1 2 3}") => (Set '(1 3 4 1 4 4 2 3 4 1 2 3)))
(test (parseS "{union {1 2 3} {4 2 3}}") => (Union (Set '(1 2 3)) (Set '(4 2 3))))
(test (parseS "{intersect {1 2 3} {4 2 3}}") => (Inter (Set '(1 2 3)) (Set '(4 2 3))))
(test (parseS "{with S {intersect {1 2 3} {4 2 3}} {union S S}}") =error> "parse-sexprS: bad `with' syntax in")
(test (parseS "{union {1 2 3}}") =error> "parse-sexprS: bad syntax in") 

;; ----------------------------------------------------
;; 3.Operations on SETs

;; Work process: Worked on one function at a time, tried to understand the logic and implemented carefully.
;; Difficulties: Main difficulties were understanding the logic inside the functions and choosing which bulit in functions to use
;;               Overcame by reading about different functions online and discussing among ourselves
;; Time: ~3-4 hours
;;-----------------------------------------------------
(: ismember? : Number SET  -> Boolean)
;; checks if a number is part of a SET (list of numbers) and returns a boolean expression
(define (ismember? n l)
  (cond [(null? l) #f] ;; if we reached the end of the list or the list is empty
        [(eq? (first l) n) #t] ;; iterate over all values and check if equals
        [else (ismember? n (rest l))]
        
        )
  )
;;;tests for ismember? function
(test (ismember? 1 '(3 4 5)) => #f)
(test (ismember? 1 '()) => #f)
(test (ismember? 1 '(1)) => #t)
(test (ismember? -1 '(1)) => #f)
(test (ismember? 1 '(-1)) => #f)



(: remove-duplicates : SET  -> SET)
;; removes all the duplicates from a list in left to right order
(define (remove-duplicates l)
  (cond
    [(null? l) l]
    [(not (ismember? (first l) (rest l)))
     (cons (first l) (remove-duplicates (rest l)))]
    [else (remove-duplicates (rest l))])
  )
 

(test (remove-duplicates '(3 4 5 1 3 4)) => '(5 1 3 4))
(test (remove-duplicates '(1)) => '(1))
(test (remove-duplicates '()) => '())
(test (remove-duplicates '(2 2 2)) => '(2))
(test (remove-duplicates '(-1 1 -1)) => '(1 -1))



(: create-sorted-set : SET -> SET)
;; removes duplicates and sorts the set from small to big
(define (create-sorted-set l)
  
  (remove-duplicates (sort l <)) ;; use sort function on a sorted set
  
  )

(test (create-sorted-set '(3 4 5)) => '(3 4 5))
(test (create-sorted-set '( 3 2 3 5 6)) => '(2 3 5 6))
(test (create-sorted-set '()) => '())
(test (create-sorted-set '(2 -1 1 -1)) => '(-1 1 2))



(: set-union : SET SET -> SET)
;; combines 2 sets, sorts the union and removes duplicates at the same time
(define (set-union A B)
  (create-sorted-set(append A B))
  )

(test (set-union '(3 4 5) '(3 4 5)) => '(3 4 5))
(test (set-union '(3 4 5) '()) => '(3 4 5))
(test (set-union '(3 4 5) '(1)) => '(1 3 4 5))
(test (set-union '(1) '(3 4 5)) => '(1 3 4 5))
(test (set-union '() '(1)) => '(1))
(test (set-union '(1 1 1) '(1)) => '(1))
(test (set-union '(1 1 1) '(-1 -1 -1)) => '(-1 1))
(test (set-union '() '()) => '())


(: set-intersection : SET SET -> SET)
; intersects 2 sets by filtering the second(B) set with function mem-filter on first set(A)
(define (set-intersection A B)
  
  (: mem-filter : Number -> Boolean)
  ; checks if a number is inside first set(A)
  (define (mem-filter n)
    (ismember? n A))
  
  (create-sorted-set(filter mem-filter B))) ; mem-filter checks if a value is inside A, so we use filter on B

(test (set-intersection '(3 4 5) '(3 4 5)) => '(3 4 5))
(test (set-intersection '(5 4 3) '(5 4 3)) => '(3 4 5))
(test (set-intersection '(3 4 5) '(3)) => '(3))
(test (set-intersection '(3 4 5) '(1)) => '())
(test (set-intersection '() '()) => '())
(test (set-intersection '() '(1)) => '())
(test (set-intersection '(-1) '(-1)) => '(-1))
(test (set-intersection '(-1) '(1)) => '())


(: set-smult : Number (Listof Number) -> SET)
; multiplies the set with a scalar 
(define (set-smult n l)
 
  (: simple-mult : Number -> Number)
  ; multiples a certain number(x) with the scalar(n)
  (define (simple-mult x)
    (* n x)
    )
  (create-sorted-set(map simple-mult l))
  )

(test (set-smult 3 '(3 4 5)) => '(9 12 15))
(test (set-smult 3 '(5 4 3)) => '(9 12 15))
(test (set-smult 2 '()) => '())
(test (set-smult 1 '(-3 -4 -5)) => '(-5 -4 -3))




;;-----------------------------------------------------
;; 4.Substitutions

;; Work process: Mainly took the same idea from the lectures and implemnted it for our needs.
;; Difficulties: No difficulties.       
;; Time: ~30 mins
#|
------------------------------------------------------
 Formal specs for `subst':
   (`set' is a { <NumList> }, E, E1, E2 are <SOL>s, `x' is some <id>,
   `y' is a *different* <id>)
      set[v/x]              = set 
      {smult n E}[v/x]      = {smult n E[v/x]}
      {inter E1 E2}[v/x]    = {inter E1[v/x] E2[v/x]}
      {union E1 E2}[v/x]    = {union E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
|#

(: substS : SOL Symbol SOL -> SOL)
(define (substS expr from to)
  (cases expr
    [(Set n) expr]
    [(Smult n s) (Smult n (substS s from to))]
    [(Inter l r) (Inter (substS l from to) (substS r from to))]
    [(Union l r) (Union (substS l from to) (substS r from to))]
    [(IdS name) (if (eq? name from)
                    to expr)]
    [(WithS bound-id named-expr bound-body)
     (WithS bound-id
            (substS named-expr from to)
            (if (eq? bound-id from) ; if we put the same value twice in same Id just return the body
                bound-body
                (substS bound-body from to)))])); else replace 'to' with 'from'

;;;tests for substS
(test (substS (Union (IdS 'x) (IdS 'x))'x (Set '(1 2 3))) => (Union (Set '(1 2 3)) (Set '(1 2 3))))
(test (substS (IdS 'x) 'x (Set '(1))) => (Set '(1)))
(test (substS (Union (IdS 'x) (IdS 'x)) 'x (Set '(1 2 3))) => (Union (Set '(1 2 3)) (Set '(1 2 3))) )
(test (substS (Inter (IdS 'x) (IdS 'y)) 'x (Set '(1 2 3))) => (Inter (Set '(1 2 3)) (IdS 'y)) )
(test (substS (WithS 'x (Set'(1 2)) (Union (IdS 'x) (Set '(5)))) 'x (Set '(1))) => (WithS 'x (Set'(1 2)) (Union (IdS 'x) (Set '(5)))))
(test (substS (WithS 'y (IdS 'x) (Union (IdS 'x) (IdS 'y))) 'x (Set '(1))) => (WithS 'y (Set '(1)) (Union (Set '(1)) (IdS 'y))) )


;;-----------------------------------------------------
;; 5.Evaluation

;; Work process: Similar to Parser part, watched and understood the content of the lectures and implemented a similiar idea 
;; Difficulties: No difficulties               
;; Time: ~1-2 hours
#|
------------------------------------------------------
Evaluation rules:
    eval({ N1 N2 ... Nl })  = sort( create-set({ N1 N2 ... Nl }))
                               ;; where create-set removes all duplications from
                                  the sequence (list) and sort is a sorting procedure
    eval({scalar-mult K E}) = { K*N1 K*N2 ... K*Nl }
                               ;; where eval(E)={ N1 N2 ... Nl }
    eval({intersect E1 E2}) = sort( create-set(set-intersection (eval(E1) , eval(E2)))     
    eval({union E1 E2})     = sort( create-set(set-union (eval(E1) , eval(E2)))
    eval({with {x E1} E2})  = eval(E2[eval(E1)/x])
    eval(id)                = error!|#

;;---------  the eval procedure ------------------------------
 
(: evalS : SOL -> SET)
;; evaluates SOL expressions by reducing them to set values
(define (evalS expr)
  (cases expr
    [(Set S) (create-sorted-set (remove-duplicates S))]  ;; sort and remove-duplicates
    [(Smult n set) (set-smult n (evalS set))]
    [(Inter l r) (set-intersection (evalS l) (evalS r))]
    [(Union l r) (set-union (evalS l) (evalS r))]
    [(WithS name named body)
     (evalS (substS body name (Set(evalS named))))]
    [(IdS name) (error 'evalS "free identifier: ~s" name)]))


;;-----------------------------------------------------
;; 6.Interface

;; Work process: It's the same as the 'run' function as seen in  lecture + pracital sessions
;; Difficulties:  We thought at first sight that we need to change the structure of this function, but after second thought
;;                it's the same. Only changing 'eval' to 'evalS' and 'parse' to 'parseS'
;; Time: ~30 mins
;;------------------------------------------------------

(: run : String -> SET)
;; evaluate a SOL program contained in a string
(define (run str)
  (evalS (parseS str)))
    
(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{union {1 2 3} {4 2 3}}") => '(1 2 3 4))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {with {x {4 5 7 6 9 8 8 8}}
                    {union x S}}}")
      => '(2 3 4 5 6 7 8 9))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
              {union {scalar-mult 3 B}
                 {4 5 7 9 8 8 8}}}")
      =error> "evalS: free identifier:")


;;==========================PART B======================================
;;-----------------------------------------------------
;; Identifing free instances

;; Work process: Our main source is practical session #7 - in which you can see containsFreeInstance function.
;;               In addition, we had to add here the relevant parts from WAE grammer   
;; Difficulties: At first look, we thought using containsFreeInstance function will be simple.
;;               However, this part was pretty messy, and it took a while to understand how to write the 'simple' solution:

;;               1. Realize that the skeleton of the function freeInstanceList is very similar to subst
;;               2. Where to put the function which removes duplicates?
;;               3. How to deal with (With bound-id named-expr bound-body)
;;               4. Is it necessary to use Fun or Call key words
;; Time: ~4-5 hours
;;-----------------------------------------------------
#|
<WAE> ::= <num> 
   | {+ <WAE> <WAE>}
   | {-  <WAE> <WAE>}
   | {* <WAE> <WAE>}
   | {/ <WAE> <WAE>}
   | {with {<id> <WAE>} <WAE>}
   | <id>
|#


(define-type WAE
  [Num  Number]
  [Add  WAE WAE]
  [Sub  WAE WAE]
  [Mul  WAE WAE]
  [Div  WAE WAE]
  [Id   Symbol]
  [With Symbol WAE WAE]
  )

;;;Parser for WAE - learnt in Lecture & practical session
(: parse-sexpr : Sexpr -> WAE) 
(define (parse-sexpr sexpr) 
  (match sexpr 
    [(number: n) (Num n)] 
    [(symbol: name) (Id name)] 
    [(cons 'with more) 
     (match sexpr 
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))] 
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])] 
    [(list '+ lhs rhs) (Add  (parse-sexpr lhs) (parse-sexpr rhs))] 
    [(list '- lhs rhs) (Sub  (parse-sexpr lhs) (parse-sexpr rhs))] 
    [(list '* lhs rhs) (Mul  (parse-sexpr lhs) (parse-sexpr rhs))] 
    [(list '/ lhs rhs) (Div  (parse-sexpr lhs) (parse-sexpr rhs))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> WAE)
;; parses a string containing a WAE expression to a WAE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))


;;;subst for WAE
(: subst : WAE Symbol WAE -> WAE)
(define (subst expr from to)
  (cases expr
    [(Num n) expr]
    [(Add l r) (Add (subst l from to) (subst r from to))]
    [(Sub l r) (Sub (subst l from to) (subst r from to))]
    [(Mul l r) (Mul (subst l from to) (subst r from to))]
    [(Div l r) (Div (subst l from to) (subst r from to))]
    [(Id name) (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body)
     (With bound-id
           (subst named-expr from to)
           (if (eq? bound-id from) bound-body 
               (subst bound-body from to)))]))

;Source - practical session #7
;definition to freeInstance in lecture #6
#|Free instance (or free occurance): An identifier that is not contained in the scope
  of any binding instance of its name is said to be 'free'|#

(: freeInstanceList : WAE -> (Listof Symbol))
(define (freeInstanceList expr)
  #|expr can be of the following figures, for each we remove duplicates while building our list|#
  (cases expr
    [(Num n) '()]
    [(Add l r) (remove-duplicates-symbol (append (freeInstanceList l) (freeInstanceList r)))]
    [(Sub l r) (remove-duplicates-symbol (append (freeInstanceList l) (freeInstanceList r)))]
    [(Mul l r) (remove-duplicates-symbol (append (freeInstanceList l) (freeInstanceList r)))]
    [(Div l r) (remove-duplicates-symbol (append (freeInstanceList l) (freeInstanceList r)))]
    [(With bound-id named-expr bound-body)
     (let ([named (freeInstanceList named-expr)]
           [body (freeInstanceList (subst bound-body bound-id (Num 0)))])
       (remove-duplicates-symbol(append named body)) )]
    [(Id name)(list name)]));We don't need here to use Fun and Call features as shown in practical session #9


(: ismember-symbol? : Symbol (Listof Symbol)  -> Boolean)
;; checks if a symbol is part of a List (list of symbols) and returns a boolean expression
(define (ismember-symbol? n l)
  (cond [(null? l) #f] ;; if we reached the end of the list or the list is empty
        [(eq? (first l) n) #t] ;; iterate over all values and check if equals
        [else (ismember-symbol? n (rest l))]
        
        )
  )

;;;tests for ismember-symbol?
(test (ismember-symbol? 'x '(x y z)) => #t)
(test (ismember-symbol? 'yy '(yy zz yy)) => #t)
(test (ismember-symbol? 'x '()) => #f)
(test (ismember-symbol? 'xyz '(xyz)) => #t)



(: remove-duplicates-symbol : (Listof Symbol)  -> (Listof Symbol) )
;; removes all the duplicates from a list in left to right order
(define (remove-duplicates-symbol l)
  (cond
    [(null? l) l]
    [(not (ismember-symbol? (first l) (rest l)))
     (cons (first l) (remove-duplicates-symbol (rest l)))]
    [else (remove-duplicates-symbol (rest l))])
  )

;;;tests for remove-duplicates-symbol
(test (remove-duplicates-symbol '(a b a)) => '(b a))
(test (remove-duplicates-symbol '(a)) => '(a))
(test (remove-duplicates-symbol '()) => '())


;;;tests for freeInstanceList
(test (freeInstanceList (parse "w")) => '(w))
(test (freeInstanceList (parse "{with {xxx 2} {with {yyy 3} {+{- xx y} z}}}")) => '(xx y z))
(test (freeInstanceList (With 'x (Num 2) (Add (Id 'x) (Num 3)))) => '())
(test (freeInstanceList (With 'x (Num 2) (Mul (Id 'x) (Num 3)))) => '())
(test (freeInstanceList (With 'x (Num 2) (Div (Id 'y) (Num 3)))) => '(y))
(test (freeInstanceList (With 'x (Num 2)(With 'x (Num 2) (Sub (Id 'y) (Id 'z))))) => '(y z)) ; test for giving a certain Id the same value twice
(test (freeInstanceList (parse "{+ z {+ x z}}")) => '(x z))
(test (freeInstanceList (parse "{/ z {* x z}}")) => '(x z))
(test (freeInstanceList (parse "{/ z {* z}}")) =error> "bad syntax in (* z)" )
(test (freeInstanceList (parse "{with x 4 {* x 5}}")) =error> "bad `with' syntax in" )

   



