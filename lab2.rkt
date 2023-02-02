#lang eopl

;;-------------------------------------------------------------------------------
;; Name: 
;; Pledge: I pledge I have abided by the Stevens Honor System.
;;-------------------------------------------------------------------------------


;; This lab is an introduction to using lists in Scheme.
;; All lists in Scheme are linked lists,
;;   meaning lists are either null (empty),
;;   or a pair of a head (the first element)
;;   and tail (the rest of the list).
;; That means you can only directly access the first element of a list!
;; To access subsequent elements you have to repeatedly access tails.
;;
;; Scheme has the tick operator ' also called "quote".
;; The tick operator is used to tell the interpreter to treat a symbol
;; as a literal rather than evaluating it as an expression.
;; When you quote a list, it quotes each sub-expression in the list,
;;   allowing you to create nested lists with just one tick mark.


;; Building Lists

'()  ;; <- This is the empty list

;; To build a list of multiple values, use the "list" function.
;; The code below will return the list (1 2 3).
(list 1 2 3)

;; The code below will return the list (a b).
(list 'a 'b)
;; By putting ticks before a and b, they are treated as literals
;;   rather than as variables.
;; Note: '(a b c d) is the same as (list 'a 'b 'c 'd).

;; To check if a list is empty, use the "null?" function.
;; Type signature: (null? list) -> boolean

;; To check how many elements are in a list, use "length".
;; Type signature: (length list) -> integer 

;; To reverse the order of elements in a list, use "reverse".
;; Type signature: (reverse list) -> list

;; To add one element to the beginning of a list, use "cons".
;; Type signature: (cons element list) -> list

;; To combine two lists, use "append".
;; Type signature: (append list list) -> list

;; To get the first element of a list, use "car".
;; Type signature: (car list) -> element

;; To get the tail of a list (everything except the first element), use "cdr".
;; Type signature (cdr list) -> list

;; NOTE: car and cdr throw exceptions when handed empty lists.





;; Implement the function "location" which accepts three arguments: a city, a state, and a country
;; and returns a *list* consisting of the given city, state, and country.
;; Example: (location "Hoboken" "New Jersey" "United States") -> ("Hoboken" "New Jersey" "United States")

;; Type signature: (name string string) -> string-list
(define (location city state country)
   (list city state country)
  )




;; Implement the function "get-state" which takes as its argument a location, i.e., a list containing
;; '(city state country) and returns the state.

;; Type signature: (state '(string string string)) -> string
(define (get-state location)
 (cadr location)
  )



;; In Scheme you can have lists of lists, a.k.a. "nested lists".
;; We'll represent a "stock" with a nested list of the following structure:

(define example-stock
  '((cusip-symbol ticker)
    (day month year)    ; year established
    (company-name ceo)
    ((street-number street) (city state zip)) ; where headquartered
    (market-cap (open-price close-price) volume)
    (eps1 eps2 eps3 eps4))) ; earnings per share for the most recent 4 years


;; Here are two example stocks to test your functions with:

(define stock1
  '(("594918104" "MSFT")
    (4 "April" 1975)
    ("Microsoft Corporation" "Satya Nadella")
    ((1 "Microsoft Way") ("Redmond" "Washington" "98053"))
    (1.847e12 (248.99 249.82) 26457493)
    (9.01 9.65 8.05 5.76)))

(define stock2
  '(("38259P508" "GOOG")
    (4 "September" 1998)
    ("Alphabet Inc." "Sundar Pichai")
    ((160 "Amphitheatre Parkway") ("Menlo Park" "California" "92210"))
    (1.216e12 (99.05 101.58) 29020354)
    (5.04 5.61 2.93 2.46)))

;; Based on the stock template, complete the following definitions
;;   using nested car and cdr calls.

;; Example:
(define (day-established stock)
  (car (cdr stock)))
;; Since the day the company was established is the second element in the stock's list, we can access it
;;   by dropping the first element (by using cdr),
;;   then using car to get the first remaining element.
;; To test these functions you can call, for example, (day-established stock1) and that should return (4 "April" 1975).

;; NOTE: Racket has extra functions for shorthands of nested car's and cdr's,
;;    so the birthday function body could also be written as (cadr student).
;; A shorthand function exists for every permutation of up to 4 car's and cdr's. Here they all are:
;;    https://docs.racket-lang.org/reference/pairs.html#%28part._.Pair_.Accessor_.Shorthands%29



;; Now implement the following functions:

;; Returns the cusip-number of the stock
(define (cusip-number stock)
   (car stock)
  )

;; Returns '((street-number street) (city state zip))
(define (address stock)
   (car (cdr (cdr (cdr stock))))
  )

;; Returns the volume field
(define (volume stock)
   (cdr (cdr (car (cdr (reverse stock)))))
  )

;; Returns the 'state field of the stock's headquarters address
(define (state stock)
   (car (cdr (car (cdr (car (cdr (cdr (reverse stock))))))))
)

;; Implement the function "pig-latin".
;; It follows the rules of Pig Latin, so
;; (pig-latin '(h a p p y)) => '(a p p y h a y)
;; (pig-latin '(b i r t h d a y)) => '(i r t h d a y b a y)

;; Assume that you only need to remove the first letter of the word,
;;   and that the word will not be empty.

;; You can use this variable "ay" instead of '(a y) in your definition
(define ay '(a y))

(define (pig-latin word)
    (append (append (cdr word) (list (car word))) ay)
)

;; Implement the function "scramble" that takes a list of three words
;; and attemps to scramble it so that the sentence is difficult to understand
;; (i.e. (w1 w2 w3) becomes (w2 w3 w1)) using the functions you learned above.

;; Examples:
;; (scramble '(I am Groot)) => '(am Groot I)
;; (scramble '(I love racket)) => '(love racket I)
;; (scramble '(Make no mistake)) => '(no mistake Make)

(define (scramble 3-words)
  (append (append (list (car (cdr 3-words))) (list (car (cdr (cdr ))))))
)
;; Created January 2018 by Samuel Kraus and Edward Minnix
;; Updated January 2020 by Jared Pincus
;; Updated January 2022 by Jacek Ossowski
