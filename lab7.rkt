#lang racket
;;-------------------------------------------------------------------------------
;; Name: 
;; Pledge: 
;;-------------------------------------------------------------------------------

;; TWO IMPORTANT NOTES:
;; 1) Returning a value from a function is *not* the same as printing it. If you
;; print a value instead returning it, your output may look the same but the comparison
;; with the expected output will fail (and you will loose points).
;;
;; 2) If you do not know how to implement a function, please do not remove its original
;; definition. Simply leave it in its original state.

;;--------------------- LAB 8 ASSIGNMENT ----------------------------------------
;; In this lab, you will write various functions which generate pairs of objects that
;; are in certain equivlance relations.
;;
;; (NOTE: references to "pairs" in this lab refer to ordered lists of two objects,
;;        not the Scheme data structure of pairs.)
;;
;; We will consider relations on two types of sets: some will be defined the set of numbers
;;
;;                     {-n, ..., -1, 0, 1, ..., n}
;;
;; and others on the set of grid points Z+ x Z+ (hence the Cartesian product):
;;
;;                  {-n, ..., -1, 0, 1, ..., n} x {-n, ..., -1, 0, 1, ..., n}
;;
;;
;; In the "HELPER FUNCTIONIS FROM LABS YOU HAVE ALREADY COMPLETED" section at the end,
;; you will find some useful set- and Cartesian product- functions that you can use
;; in your implementations. There should be no need to modify the functions found there.
;;
;; You should not need to modify the first three functions below which are provided for your convenience
;; to plot sets of points on the coordinate plane. You can use those functions to visualize the
;; relations and equivalence classes on the coordinate plane.

;; ------------------------ PLOTTING HELPER FUNCTIONS ---------------------

(require plot)

;; plot-2d-points draws a list of points on the coordinate plane with the
;; coordinates given in the point-list argument. Try
;; (plot-2d-points '( (1 2) (2 4) (3 6) (4 8) (5 10)))
#|
.
|#
(define (plot-2d-points point-list)
  (define xmin (- (apply min (map car point-list)) 1))
  (define xmax (+ (apply max (map car point-list)) 1))
  (define ymin (- (apply min (map cadr point-list)) 1))
  (define ymax (+ (apply max (map cadr point-list)) 1))
  (plot (points point-list #:color 'red
                       #:x-min xmin #:x-max xmax
                       #:y-min ymin #:y-max ymax))
  )

;; generate-graph-specs is an auxiliary function you do not need to use.
(define (generate-graph-specs list-of-sets)

       [if (null? list-of-sets)
           '()
           ( let ()
                (define point-list (car list-of-sets))
                (define xmin (- (apply min (map car point-list)) 1))
                (define xmax (+ (apply max (map car point-list)) 1))
                (define ymin (- (apply min (map cadr point-list)) 1))
                (define ymax (+ (apply max (map cadr point-list)) 1))
                (append (list (points point-list    #:color (list-ref '(black red blue green) (modulo (length list-of-sets) 4))
                                              #:x-min xmin #:x-max xmax
                                              #:y-min ymin #:y-max ymax
                                              #:sym 'fullcircle1
                                              #:size 20))
                        (generate-graph-specs (cdr list-of-sets))))    
      ]
  )

;; plot-2d-sets plots sets of grid points and draws each set in different color.
;; The sets are given in a list of lists. Each set is drawn in a different color.
;; Useful for drawing equivalence classes but do not confuse it with plot-2d-points.
;; Try this:
;;  (plot-2d-sets '[ ((1 2) (2 4) (3 6) (4 8) (5 10))   ( (1 1) (2 2) (3 3) (4 4) (5 5))  ])
#|
.

|#
(define (plot-2d-sets list-of-sets)
    (plot (append (list (axes)) (generate-graph-specs list-of-sets))
          #:width 800
          #:height 800)

  )

;;------------------------------ END OF PLOTTING FUNCTIONS --------------------



;;----------------------------- LAB 7 ASSIGNMENT STARTS HERE ------------------
;;
;; Please use #lang racket on top of this file.
;;
;; (generate-domain n) should simply create a list of integers between (and including)
;; -n and n. It is not important to have those integer sorted (if you insist on having
;; the integers sorted, you can use Scheme's sort function at the end). For example, it
;; is totally fine to have generate-domain work as follows:
;;
;; (generate-domain 3) --> '(0 -1 1 -2 2 -3 3)
;; (generate-domain 5) --> '(0 -1 1 -2 2 -3 3 -4 4 -5 5)
;; (generate-domain 1) --> '(0 -1 1)
;; (generate-domain 0) --> '(0)
;;
;; The argument is assumed to be a non-negative integer. You can check whether your
;; output is correct by calling
;; (plot-2d-points (cartesian-product2 (generate-domain 4) '(0)) )
#|
.
|#
;;
;; (Cartesian "multiplication" by 0 is added so that the integers from [generate-domain 4] acquire
;; the second coordindate 0 and can be plotted using our helper function plot-2d-points.)

;; (* n -1)
;; (append (list n) (list (- n))) (generate-domain (- n 1)) (* n -1))

(define (generate-domain n)
   (cond [(eqv? n 0) `(0)]
         [else (append (append (generate-domain (- n 1)) (list (*  n -1)))(list n))]
     )
  )


;;  (generate-2d-domain n) should generate all grid points with coordinates
;; -n <= x <= n and -n <= y <= n. In other wordsb  the Cartesian product of
;; (-n ... -1 0 1 ... n) x (-n ... -1 0 1 ... n).
;; Again, the grid points do not have to be given in any particular order.
;; For example,
;; (generate-2d-domain 0) --> '((0 0))
;; (generate-2d-domain 1) --> '((0 0) (0 -1) (0 1) (-1 0) (-1 -1) (-1 1) (1 0) (1 -1) (1 1))
;; (generate-2d-domain 2) --> '((0 0) (0 -1) (0 1) (0 -2) (0 2) (-1 0) (-1 -1) (-1 1) (-1 -2) (-1 2) (1 0) (1 -1) (1 1) (1 -2) (1 2) (-2 0) (-2 -1) (-2 1) (-2 -2) (-2 2) (2 0) (2 -1) (2 1) (2 -2) (2 2))
;;
;; To check whether your implementation behaves as expected, try plotting the generated points
;; with
;; (plot-2d-sets (list (generate-2d-domain 2) ))
;;
;; (plot-2d-sets takes a list-of-lists as an argument -- hence the use of the list function).


(define (generate-2d-domain n)
  (cond [(= n 0) `((0 0))]
        [else (cartesian-product (list (generate-domain n) (generate-domain n)))]
    )
  )

;;(list (generate-domain n))


;; (filter predicate relation) is intended to generate all pairs of integers in
;; the domain (-n ... -1 0 1 ... 0) which belong to a relation defined by
;; the given predicate. We will also consider the domain of all pairs of numbers
;; from (-n ... -1 0 1 ... 0). To see whether you are getting the expected results, you
;; can plot the obtained points. For example, if defined a relation
;;
;;  a R b <=> ceiling(a/3) = ceiling(b/3)
;;
;; which is captured by the anonymous function
;;
;; (lambda (a b) (= (ceiling (/ a 3)) (ceiling (/ b 3)))),
;;
;; then in the square (-5, 5) x (-5, 5) you would obtain the following pairs
;;
#|
.

The above image is generated by the following code
(define relation-pairs (filter (lambda (a b) (= (ceiling (/ a 3)) (ceiling (/ b 3)))) (generate-2d-domain 5)))
(plot-2d-sets (list relation-pairs))

The relation argument is just a set of pairs of objects under consideration (objects in our case will be either integers or pairs of integers)
Examples:
(filter (lambda (a b) (> a b) ) (generate-2d-domain 2)) --> '((0 -1) (0 -2) (-1 -2) (1 0) (1 -1) (1 -2) (2 0) (2 -1) (2 1) (2 -2)) ; relation a R b <=> a > b

(filter (lambda (a b) [> (abs a) (abs b)] ) (generate-2d-domain 2)) --> '((-1 0) (1 0) (-2 0) (-2 -1) (-2 1) (2 0) (2 -1) (2 1))   ; relation a R b <=> |a| > |b|

(filter (lambda (a b) [< (abs a) (sqrt (abs b))] ) (generate-2d-domain 2)) --> '((0 -1) (0 1) (0 -2) (0 2) (-1 -2) (-1 2) (1 -2) (1 2))  ; relation a R b <=> |a| < sqrt(|b|)

Below a, b are pairs of integers (i.e., points on the 2d grid). We can access a, b's coordinates by the car and cadr functions.

(define (my-pred a b) [and (> (car a) (* 2 (abs (car b)))) (> (cadr a) (* 2 (abs (cadr b))))] ) ; relation (p, q) R (x, y) <=> p > 2|x| and q > 2|y|

(filter my-pred (cartesian-product2  (generate-2d-domain 2) (generate-2d-domain 2))) --> '(((1 1) (0 0)) ((1 2) (0 0)) ((2 1) (0 0)) ((2 2) (0 0)))

The domain for the above relation is Z^2 so we need to generate a set of pairs of grid points; hence the use of cartesian-product2
|#

;;(predicate (car relation) (cons (car relation) (filter predicate (cdr relation))))

(define (filter predicate relation)
  (cond [(null? relation) '()]
        [(eq? (predicate (car (car relation)) (cadr (car relation))) #t) (append (list (car relation)) (filter predicate (cdr relation)))]
        [else (filter predicate (cdr relation))])
  )

#|
(generate-all-relation-pairs dim predicate n) simply automates what we did in the examples for the implementation of
(filter predicate relation) .

1) It takes the dimension as an argument which can only be 1 or 2.
2) The predicate argument is a condition which specifies whether objects under consideration are in the relation.
3) n is the size of the domain:
        if dim = 1, then the domain (-n ... -1 0 1 ... n) should be generated and *pairs* from that domain should be checked
                    for membership in the relation
        if dim = 2, then the domain (-n ... -1 0 1 ... n) x (-n ... -1 0 1 ... n) and *pairs* from that domain should be checked
                    for membership in the relation

Examples:

Automation of the first example in the previous implementation:

(generate-all-relation-pairs 1 [lambda (a b) (> a b)]  2) --> '((0 -1) (0 -2) (-1 -2) (1 0) (1 -1) (1 -2) (2 0) (2 -1) (2 1) (2 -2)) ; relation a R b <=> a > b

Automation of the *third* example in the previous implementation:

(generate-all-relation-pairs 1  (lambda (a b) [< (abs a) (sqrt (abs b))]) 2) --> '((0 -1) (0 1) (0 -2) (0 2) (-1 -2) (-1 2) (1 -2) (1 2))  ; relation a R b <=> |a| < sqrt(|b|)

Automation of the fourth examle above

(define (my-pred a b) [and (> (car a) (* 2 (abs (car b)))) (> (cadr a) (* 2 (abs (cadr b))))] ) ; relation (p, q) R (x, y) <=> p > 2|x| and q > 2|y|

(generate-all-relation-pairs 2 my-pred  2)  --> '(((1 1) (0 0)) ((1 2) (0 0)) ((2 1) (0 0)) ((2 2) (0 0)))
|#

(define (generate-all-relation-pairs dim predicate n)
  (cond [(= dim 1) (filter predicate (generate-2d-domain n))]
        [(= dim 2) (filter predicate (cartesian-product2 (generate-2d-domain n) (generate-2d-domain n)))]
        [else `(())])
  )


#|
This is probably the most difficult function to implement you've encountered so far.
1) a predicate is given in the argument which is assumed to define an equivalence relation
2) e is an element in the domain under consideration (either (-n ... -1 0 1 ... n) or (-n ... -1 0 1 ... n)^2 )
3) "classes" is a set of equivalence classes of the relation given by the predicate

(add-to-proper-class predicate e classes) must find an equivalence class to which e belongs and add e to it.
Or, if no such class exists e should form its own class (e) which then should be added to the argument classes.

Examples:

(add-to-proper-class (lambda (a b) [= (modulo (- a b) 3) 0]) 7  '[(3 6 9 12) (5 11 14)]  ) -> '((3 6 9 12) (5 11 14) (7)) ; relation a R b <=> a - b is a multiple of 3

(add-to-proper-class (lambda (a b) [= (modulo (- a b) 3) 0]) 7  '[(3 6 9 12) (4 10 13) (5 11 14)]  ) -> '((3 6 9 12) (4 10 13 7) (5 11 14)) ; relation a R b <=> a - b is a multiple of 3


Here we have equivalence classes of relation defined on integer grid points p, q (i.e., p, q are pairs of integers)

(define (my-pred p q) [= (+ (car p) (cadr p)) (+ (car q) (cadr q))] )  ; relation (p1 p2)  R (q1 q2) <=> p1 + p2 = q1 + q2

(add-to-proper-class my-pred '(5 3) '[ ((5 2) (4 3) (3 4) (2 5))  ((1 7) (2 6) (4 4)) ] ) --> '(((5 2) (4 3) (3 4) (2 5)) ((1 7) (2 6) (4 4) (5 3)))

|#

(define (add-to-proper-class predicate e classes)
 (cond [(null? classes) (list (list e))]
       [(predicate (car (car classes)) e) (append (list (append (car classes) (list e))) (cdr classes))]
       [else (append (list (car classes)) (add-to-proper-class predicate e (cdr classes)))])
  )

#|
Finally, (find-equivalence-classes predicate domain) should partition the given domain into equiva]lence classes
under the relation defined by the predicate (the relation is assumed to be an equivalence relation)

(find-equivalence-classes  (lambda (a b) [= (modulo (- a b) 3) 0]) (generate-domain 10)) --> '((10 -8 7 -5 4 -2 1) (-10 8 -7 5 -4 2 -1) (9 -9 6 -6 3 -3 0)) ; relation a R b <=> a - b is a multiple of 3

(find-equivalence-classes (lambda (a b) (= (ceiling (/ a 3)) (ceiling (/ b 3)))) (generate-domain 10))
 --> '((10) (-10 -9) (9 8 7) (-8 -7 -6) (6 5 4) (-5 -4 -3) (3 2 1) (-2 -1 0)) ; relation  a R b <=> ceiling(a/3) = ceiling(b/3)

Here let's generate equivalence classes on the set of integer grid points under the relation: (p1 p2)  R (q1 q2) <=> p1 + p2 = q1 + q2
 (find-equivalence-classes (lambda (a b)  (= (+ (car a) (cadr a)) (+ (car b) (cadr b))))   (generate-2d-domain 4))
-->
'(((4 4))
  ((4 -4) (-4 4) (3 -3) (-3 3) (2 -2) (-2 2) (1 -1) (-1 1) (0 0))
  ((4 3) (3 4))
  ((4 -3) (3 -2) (-3 4) (2 -1) (-2 3) (1 0) (-1 2) (0 1))
  ((4 2) (3 3) (2 4))
  ((4 -2) (3 -1) (2 0) (-2 4) (1 1) (-1 3) (0 2))
  ((4 1) (3 2) (2 3) (1 4))
  ((4 -1) (3 0) (2 1) (1 2) (-1 4) (0 3))
  ((4 0) (3 1) (2 2) (1 3) (0 4))
  ((-4 -4))
  ((-4 3) (3 -4) (-3 2) (2 -3) (-2 1) (1 -2) (-1 0) (0 -1))
  ((-4 -3) (-3 -4))
  ((-4 2) (-3 1) (2 -4) (-2 0) (1 -3) (-1 -1) (0 -2))
  ((-4 -2) (-3 -3) (-2 -4))
  ((-4 1) (-3 0) (-2 -1) (1 -4) (-1 -2) (0 -3))
  ((-4 -1) (-3 -2) (-2 -3) (-1 -4))
  ((-4 0) (-3 -1) (-2 -2) (-1 -3) (0 -4)))

Let's depict two of the above classes (second and fourth)

(plot-2d-sets '[  ((4 -4) (-4 4) (3 -3) (-3 3) (2 -2) (-2 2) (1 -1) (-1 1) (0 0))  ((4 -3) (3 -2) (-3 4) (2 -1) (-2 3) (1 0) (-1 2) (0 1))  ])

.

To implement find-equivalence-classes you could use recursion by "first partitioning
the previous set into equivalence classes and then adding the next element to the proper
class among those that you have just found." Hence you will repeatedly use the function
 add-to-proper-class on the output of the recursive call to find-equivalence-classes.
I used quotation marks because you will need to figure out what that all this means.
|#

(define (find-equivalence-classes predicate domain)
  (cond
    [(null? domain) '(())]
    [else (add-to-proper-class predicate (car domain) (find-equivalence-classes predicate (cdr domain)))])
  )


;;_______________HELPER FUNCTIONIS FROM LABS YOU HAVE ALREADY COMPLETED_____________________________

;; Below are helper functions you may utilize for the functions you write!


;; Returns e ∈ L.
;; Type signature: (element? item list) -> boolean
(define (element? e L)
  (member e L))

;; Returns L as a set (removes duplicates).
;; Type signature: (make-set list) -> set
(define (make-set L)
  (cond [(null? L) '()]
        [(member (car L) (cdr L)) (make-set (cdr L))]
        [else (cons (car L) (make-set (cdr L)))]))

;; Returns the set of LA unioned with the set of LB.
;; Type signature: (union list list) -> set
(define (union LA LB)
  (make-set (append LA LB)))

;; Returns the set of LA intersected with the set of LB.
;; Type signature: (intersection list list) -> set
(define (intersection LA LB)
  (make-set (intersection-helper LA LB)))
(define (intersection-helper LA LB)
  (cond [(null? LA) '()]
        [(element? (car LA) LB)
         (cons (car LA) (intersection-helper (cdr LA) LB))]
        [else (intersection-helper (cdr LA) LB)]))

;; Returns SA ⊆ SB.
;; Type signature: (subset? set set) -> boolean
(define (subset? SA SB)
  (cond [(null? SA) #t]
        [(element? (car SA) SB)
         (subset? (cdr SA) SB)]
        [else #f]))

;; Returns whether SA and SB contain the same elements.
;; Type signature: (set-equal? set set) -> boolean
(define (set-equal? SA SB)
  (and (subset? SA SB)
       (subset? SB SA)))

;; Returns the difference of LA as a set and LB as a set.
;; Type signature: (set-difference list list) -> set
(define (set-difference LA LB)
  (make-set (set-difference-helper LA LB)))
(define (set-difference-helper LA LB)
  (cond [(null? LA) '()]
        [(element? (car LA) LB)
         (set-difference-helper (cdr LA) LB)]
        [else (cons (car LA)
                    (set-difference-helper (cdr LA) LB))]))

;; Returns the symmetric difference of LA as a set and LB as a set.
;; Type signature: (sym-diff list list) -> set
(define (sym-diff LA LB)
  (union (set-difference LA LB)
         (set-difference LB LA)))

;; Returns the cardinality of L as a set.
;; Type signature: (cardinality list) -> int
(define (cardinality L)
  (length (make-set L)))

;; Returns whether sets SA and SB are disjoint.
;; Type signature: (disjoint? set set) -> boolean
(define (disjoint? SA SB)
  (null? (intersection SA SB)))

;; Returns SA ⊇ SB.
;; Type signature: (superset? set set) -> boolean
(define (superset? SA SB)
  (subset? SB SA))

;; Returns the set of L, with e added to it.
;; Type signature: (insert element list) -> set
(define (insert e L)
  (make-set (cons e L)))

;; Returns set S without element e.
;; Type signature: (remove element set) -> set
(define (remove e S)
  (set-difference S (list e)))



(define (distinct-elems? tuple)
     (= (length (make-set tuple)) (length tuple))
    )


;; Given a positive integer k and a list, return a list
;; whose elements are k copies of the original list
;;
;; (repeat 3 '(7 10 6)) -> '((7 10 6) (7 10 6) (7 10 6))
;;  (repeat 1 '(2 3 9)) -> '((2 3 9))
;; (repeat 4 '(1)) -> '((1) (1) (1) (1))

(define (repeat k lst)

   [ if (= k 0)
        '()
         (append (list lst) (repeat (- k 1) lst))
       ]
  )

;; Write a "robust" version of the cons function. Namely, when the "object" argument
;; below is a list, then the function returns that list with the pre-pended x (hence
;; in this case rcons is indistinguishable from cons. 
;; If the "object" argument is not a list then rcons should return the list (x object)
;;
;; (rcons 4 '(7 9 5)) -> '(4 7 9 5)
;; (rcons 4 2) -> '(4 2)
;; (rcons '(1 2) '(7 8 9)) -> '((1 2) 7 8 9)
;;

(define (rcons x object)
      (if (list? object)
           (cons x object)
           (list x object))
  )


;; The below function shold take an object x as an argument and a list lst = (e1 ..., en).
;; pair-up should create a list of pairs ((x e1) (x e2) ... (x en)) 
;;
;; (pair-up 3 '(10 11 12)) -> '((3 10) (3 11) (3 12))
;; (pair-up 3 '((3 (1 2)) (3 (7 6)) (3 (11 12)))
;; (pair-up 3 '((3 (1 2)) (3 33) (3 (11 12)))


(define (pair-up x lst)

   [if (eq? lst '())
       '()
       (append (list (list x (car lst))) (pair-up x (cdr lst)))
       ]
         
  )

;; The below function should take as arguments two lists and return their
;; Cartesian product ("2" means here that only 2 lists will be worked with).
;; It is very usefull to use the above pair-up function when implementing
;; cartesian-product2. Cartesian product involving an empty list should return
;; an empty list.
;;
;; (cartesian-product2 '(1 2 3) '(a b)) -> '((1 a) (1 b) (2 a) (2 b) (3 a) (3 b))
;; (cartesian-product2 '() '(10 11 12)) -> '()
;; (cartesian-product2 '(a b c) '(10 11 12)) -> '((a 10) (a 11) (a 12) (b 10) (b 11) (b 12) (c 10) (c 11) (c 12))
;; Below the sets whose product is generated contain integer 2d grid points
;; (cartesian-product2 '((1 2) (3 4)) '((7 8) (9 10))) -> '(((1 2) (7 8)) ((1 2) (9 10)) ((3 4) (7 8)) ((3 4) (9 10)))

(define (cartesian-product2 lst1 lst2)

   [if (eq? lst1 '())
        '()
        (append (pair-up (car lst1) lst2) (cartesian-product2 (cdr lst1) lst2))
        ]
  )

;; The below function should return Cartesian product of several lists.
;; The first argument is a list of lists whos product will be calculated. When the
;; number of given lists is < 2 return '(). One possible way of implementing this
;; function is to 1) return cartesian-product2 when the number of lists is 2, and
;; 2) when list-of-lists = (l1 l2 ... ln), return
;;        (cartesian-product2 l1 (cartesian-product (l2 ...ln)))
;;
;; The function should return '() when any of the lists is empty.
;; NOTE for Lab07: this version of cartesian product is slightly different and is really
;; meant to be used when we calculate a product of just 2 sets (for Lab07 that's all one needs).
;;
;;
;; Below two sets of integer grid points are  x-crossed. The two sets are given in a list of lists.
;;  (cartesian-product '[  ((1 2) (3 4)) ((7 8) (9 10))  ])
;; If one wants one could use this version of cartesian-product for three or more sets but the results
;; will be slightly different from those before.
;; (cartesian-product '( (1 2 3) (x y) (a b))) -> '((1 (x a)) (1 (x b)) (1 (y a)) (1 (y b)) (2 (x a)) (2 (x b)) (2 (y a)) (2 (y b)) (3 (x a)) (3 (x b)) (3 (y a)) (3 (y b)))
;; (cartesian-product '( (1 2 3) (x y) ())) -> '()
;; (cartesian-product '( ("A") (x y) ("C"))) -> '(("A" (x "C")) ("A" (y "C")))
;;
;;
(define (cartesian-product list-of-lists)

      [if  (< (length list-of-lists) 2)
           '()
           [if (= (length list-of-lists) 2)
                (cartesian-product2 (car list-of-lists) (cadr list-of-lists))
                ( let [ (l1 (car list-of-lists)) (other-lists (cdr list-of-lists))]
                        (cartesian-product2 l1 (cartesian-product other-lists))
                )
            ]
       ]
  )
