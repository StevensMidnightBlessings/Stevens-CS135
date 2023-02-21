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
