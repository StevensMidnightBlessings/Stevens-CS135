#lang racket
;;-------------------------------------------------------------------------------
;; Name: 
;; Pledge: I pledge my honor I have abided by the Stevens Honor System.
;;-------------------------------------------------------------------------------

;; In this lab you will write code in steps that generates the power set of a given A.
;; You will need to specify #lang racket as the first line of your file.

;; First we need to be able to convert a given non-negative integer into a list of
;; its digits in the binary representation. This can be accomplished in two ways:
;;
;; Method 1:
;;
;; 1) convert a positive integer to binary string (use the number->string function with the base of 2)
;; 2) convert string to a list of characters (use the string->list function )
;; 3) use the map function to convert each character in the list into a single digit string (e.g., (string #\1) -> "1" )
;; 4) use the map function to convert a single digit string into a number (use the string->number function)
;;
;; Method 2:
;; Recursively perform the following operations:
;; 1) find the remainder mod 2 <- this will be your next digit from the end
;; 2) subtract the remainder from the original number and then divide by 2
;; 3) go to step 1)
;;
;; It is up to you which method you choose.
;;
;; Examples:
;; (cvt-to-list 0) -> '(0)
;; (cvt-to-list 1) -> '(1)
;; (cvt-to-list 2) -> '(1 0)
;; (cvt-to-list 3) -> '(1 1)
;; (cvt-to-list 4) -> '(1 0 0)
;; (cvt-to-list 7) -> '(1 1 1)
;;
;; num is assumed to be a non-negative integer.

;; (cvt-to-list 0) (cvt-to-list 1) (cvt-to-list 2) (cvt-to-list 3) (cvt-to-list 4) (cvt-to-list 7)

(define (cvt-to-list num)
   (if (= num 0) '()
      (append (cvt-to-list (quotient num 2))
              (list (remainder num 2)))
    )
  )

;;
;; pad-to-length is a function which takes as arguments
;; a list of digits and a number n. It prepends the list of digits
;; with 0s until the digit list has the length n. If the list's length is
;; >= than n, then pad-to-length does not alter the digit list.
;;
;; (pad-to-length '(1 0 1) 7) -> '(0 0 0 0 1 0 1)
;; (pad-to-length '(1 0 1) 2) -> '(1 0 1)
;;  (pad-to-length '() 10)    -> '(0 0 0 0 0 0 0 0 0 0)

;; (pad-to-length '(1 0 1) 7) (pad-to-length '(1 0 1) 2) (pad-to-length '() 10)

(define (pad-to-length digits n)
  (if (>= (length digits) n)
     digits
     (pad-to-length (cons 0 digits) n))
)


;; cvt-to-list-padded should work in the same way as cvt-to-list except that it
;; padds 0s to the left of the list if the list returned previously from cvt-to-list has
;; length smaller than the argument's length.
;;
;; Examples:
;; (cvt-to-list-padded 10 7) -> '(0 0 0 1 0 1 0)
;; (cvt-to-list-padded 10 15) -> '(0 0 0 0 0 0 0 0 0 0 0 1 0 1 0)
;; (cvt-to-list-padded 7 2) -> '(1 1 1) ; When the length asked for is shorter than the digit list, we ignore the second argument

;; (cvt-to-list-padded 10 7) (cvt-to-list-padded 10 15) (cvt-to-list-padded 7 2)

(define (cvt-to-list-padded num length)
    (pad-to-length (cvt-to-list num) length)
  )


;; pick-subset is the core function of this lab. Given a list A (interpreted as a set of some distinct elements)
;; and a bit list, one picks up elements of the set A that correspond to 1s in the bit list provided
;; as the second argument. As a result, one obtains a subset of the set A. If the bit list is shorter
;; than the set, the bit-list  is assumed to contain the suitable number of 0s at its end.  For simplicity
;; we'll assume that the bit-list will never be longer than A.
;;
;; Examples:
;; (pick-subset '(a b c d) '(1 0 1 0)) -> '(a c)
;; (pick-subset '(a b c d) '(0 1 1 1)) -> '(b c d)
;; (pick-subset '(a b c d) '(0 1)) -> '(b)
;; (pick-subset '(a b c d) '(0 0 0 0)) -> '()

;; The elements of A are assumed to be distinct.

;; (pick-subset '(a b c d) '(1 0 1 0)) (pick-subset '(a b c d) '(0 1 1 1)) (pick-subset '(a b c d) '(0 1)) (pick-subset '(a b c d) '(0 0 0 0))


(define (pick-subset A bit-list)
  (cond ((null? A) '())
        ((null? bit-list) '())
        ((= 1 (car bit-list)) (cons (car A) (pick-subset (cdr A) (cdr bit-list))))
        (else (pick-subset (cdr A) (cdr bit-list))))
  )


;; The below function takes a set A (a list of distinct elements) and a non-negative number n.
;; It then counts from n down to 0 in binary generating subsets corresponding to digits of
;; n, n-1, n-2, ..., 1, 0 in their binary representation.
;; For example, counting from 8 to 0 would geneerate bit lists
;;
;; 1 0 0 0
;; 0 1 1 1
;; 0 1 1 0
;; 0 1 0 1
;; 0 1 0 0
;; 0 0 1 1
;; 0 0 1 0
;; 0 0 0 1
;; 0 0 0 0 
;;
;; which would then be converted to subsets of the set A. If A was '(a b c d), we would obtain
;; (a)
;; '(b c d)
;; '(b c)
;; ...
;; ...
;; '(d)
;; '()
;;
;; Examples:
;;  (pick-subsets-down-from-num '(a b c d) 8) -> '((a) (b c d) (b c) (b d) (b) (c d) (c) (d) ())
;;  (pick-subsets-down-from-num '(a b c d) 4) -> '((b) (c d) (c) (d) ())
;;  (pick-subsets-down-from-num '(a b c ) 7) -> '((a b c) (a b) (a c) (a) (b c) (b) (c) ()) ; Note: those are all subsets of '(a b c)
;;
;; (pick-subsets-down-from-num '(a b c d) 8) (pick-subsets-down-from-num '(a b c d) 4) (pick-subsets-down-from-num '(a b c ) 7)


(define (pick-subsets-down-from-num A n)
  #|
      (if (< n 0) '()
            (cons (pick-subset A (cvt-to-list-padded n (length A))) (- n 1))))
  |#
  
  (if (equal? n 0)
      (list (pick-subset A (cvt-to-list-padded n (length A))))
      (cons (pick-subset A (cvt-to-list-padded n (length A))) (pick-subsets-down-from-num A (- n 1)))
      )
  )
  
;; As explained in class, one can now generate all subsets of a set A by counting down from n = 2^l - 1 to 0 where
;; l is the length of the set A. That's what the function power-set should do. Generating all binary numbers in the
;; above range will enumereate all possible 0-1 sequences which then will be used to "pick up" all possible subsets.
;; Here, each number (a bit list for us) in the range will be padded with 0s so that it has l digits.
;; To implement power-set you simply need to run pick-subsets-down-from-num with the second argument 2^l - 1.
;;
;; (power-set '(a b c d)) -> '((a b c d) (a b c) (a b d) (a b) (a c d) (apick-subsets-down-from-num '(a b c d) 8) c) (a d) (a) (b c d) (b c) (b d) (b) (c d) (c) (d) ())
;; (power-set '(1 2 3)) -> '((1 2 3) (1 2) (1 3) (1) (2 3) (2) (3) ())

;; (power-set '(a b c d)) (power-set '(1 2 3))

(define (power-set A)
  (pick-subsets-down-from-num A (- (expt 2 (length A)) 1))
 )
