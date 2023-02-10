#lang eopl

;;-------------------------------------------------------------------------------
;; Name: 
;; Pledge: I pledge my honor that I have abided by the Stevens Honor System.
;;-------------------------------------------------------------------------------

;; This lab serves as an introduction to using recursion in Scheme.
;; You should implement every function in this program recursively.
;;
;; NOTE: the empty list is a valid input for each function!
;;
;; Also, just because your code works for provided test cases
;;   doesn't mean it always works.
;; Always come up with your own test cases too!



;; Implement your version of multiplication denoted by the symbol @. @ should take as arguments
;; an integer a and a non-negative integer b. The return value of @ should be a*b. HOWEVER, the only
;; arithmetic operation you're allowed to use is +.

;; Examples:
;; (@ 7 3) -> 21
;; (@ 8 0) -> 0
;; (@ -3 4) -> -12
;; (@ 0 4) -> 0
;; (@ -5 1) -> -5

;; Type signature: (power integer integer) -> integer
(define (@ a b)
   (cond ((eqv? b 0) 0) ;;checks if b = 0, returns 0
   (else (cond ((eqv? b 1) a) ;;checks if b = 1, returns a
   (else (+ a (@ (+ b -1) a)))))) ;;recursion
  )


;; Implement the "power" function which takes as its arguments a positive integer x and a non-negative
;; integer n. "power" shoud return x raised to the power of n. HOWEVER, the only
;; arithmetic operation you're allowed to use is *.

;; Examples:
;; (power 3 2) -> 9
;; (power 2 10) -> 1024
;; (power 3 1) -> 3
;; (power 7 0) -> 1

;; Type signature: (power integer integer) -> integer

(define (power x n)
 (cond ((eqv? n 0) 1) ;;checks if n = 0, returns 1
 (else (cond ((eqv? n 1) x) ;;checks if n = 1, returns n
 (else  (* x (power x (- n 1))))))) ;;recursion 
  )


;; Implement "nth", which returns the element at index n of the provided list.
;; The first element of the list is at index 0, the second at index 1, etc.
;; You may assume that the "n" provided is non-negative and less than the list's length.

;; Examples:
;; (nth 1 '(Sandeep Owen Jared Sarvani)) -> Owen
;; (nth 5 '("zero" "one" "two" "three" "four" "five")) -> "five"
;; (nth 0 '(a b c)) -> 'a

;; Type signature: (nth integer list) -> element from list

(define (nth n lst)
   (if (null? lst) `null ;;checks for empty list and returns null
       (if (= n 0)  ;;checks if n is the first element and uses car to return
          (car lst)
    (nth (- n 1) (cdr lst)))) ;;recursion to decrement until n = 0
  )

;; Implement "sum of squares", which returns the summation of squares of all the elements in
;; the given list. You may assume that the list will only contain numbers.
;; Note: mathematical convention is that the sum of squares of nothing is 0.

;; Examples:
;; (sum2 '(1 3 5 1)) -> 36
;; (sum2 '(75050 344 0 -70 125)) -> 5632641361

;; Type signature: (sum2 number-list) -> number
(define (sum2 lst)
 (if (null? lst) 0 ;;checks if list is empty
 (+ (* (car lst) (car lst)) (sum2 (cdr lst)))) 
)


;; Implement "product", which returns the result of multiplying all elements in the list.
;; You may assume that the list will only contain numbers.
;; Note: mathematical convention is that the product of nothing is 1.

;; Examples:
;; (product (list 1 3 5 4)) -> 60
;; (product '(100 -50 6789 4183457)) -> -142007447865000

;; Type signature: (product number-list) -> number
(define (product lst)
 (if (null? lst) 1 ;;checks if list is empty
     (/ (car lst) (product (cdr lst))))
)


;; Implement the following continued fraction function denoted as "cf" and defined as follows.
;; "cf" takes as its only argument a list of positive integers. If the list has length 1, then the
;; only element of the list is returned as the function's value. Otherwise, the function returns the
;; fraction built according to the pattern below
;; (cf '(a))     ->  a
;;
;; (cf '(a b))   ->        1
;;                    a + ---
;;                         b
;;
;; (cf '(a b c)) ->               1
;;                    a    +  ------------
;;                                    1
;;                              b   + ---
;;                                     c
;;
;; (cf '(a b c d)) ->               1
;;                     a    +  ----------------
;;                                        1
;;                              b   + ---------
;;                                     c  +  1
;;                                          ---
;;                                           d
;;
;; and so on.
;;
;; Examples:

;; Approximation for Pi
;; (exact->inexact (cf '(3 7 15 1 292 1))) -> 3.141592653921421

;; Approximation for square root of 2
;; (exact->inexact (cf '(1 2 2 2 2 2 2 2 2 2 2 2 2 2 2))) -> 1.4142135623637995

;; Approximation for golden ratio
;; (exact->inexact (cf '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))) -> 1.618034447821682
;;
;; Type signature (cf number-list) -> fraction

;;(define (cf number-list)
 ;;(if (null? number-list) 0 ;;checks if empty, returns 0
  ;;(/ (car number-list)
    ;;(+ 1 (cf (cdr number-list)))))
 ;;)

(define (cf number-list)
  (if (eqv? (length number-list) 1) (car number-list)
  (+ (car number-list) (/ 1 (cf (cdr number-list)))))
  )

;; Implement "2filter", which accepts a predicate (a function that returns a boolean) and a list,
;; and returns the same list except that for every element satisfing the predicate a new copy of
;; that element is added. The new copy should be added beside the element which makes the predicate
;; return #t.

;; Examples

;; (2filter zero? '(1 0 2 34 56 1 0)) -> '(1 0 0 2 34 56 1 0 0)

;; (2filter even? '(0 1 2 3 4 5 6 7 8 9)) -> '(0 0 1 2 2 3 4 4 5 6 6 7 8 8 9)
;; (2filter number? '(shave and 1 haircut 2 bits)) -> '(shave and 1 1 haircut 2 2 bits)

;; (2filter (lambda (x) (> x 10)) '(1 0 2 34 56 1 0) ) -> (1 0 2 34 34 56 56 1 0)

;; Type signature: (2filter predicate list) -> list

(define (2filter pred lst)
   (cond ((null? lst) `())
         ((pred (car lst)) (cons (car lst) (cons (car lst)
         (2filter pred (cdr lst)))))
         (else (cons (car lst)
               (2filter pred (cdr lst)))))
 )

;; Created January 2018 by Samuel Kraus and Edward Minnix
;; Updated January 2020 by Jared Pincus
;; Modified Januare 2022 by Jacek Ossowski
