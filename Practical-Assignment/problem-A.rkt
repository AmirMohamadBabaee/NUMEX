#lang racket

(define l1 (list 1 2 3 4))
(define l2 (list 4 3 1 5))

(define (inversion-helper item item-list)
  (cond
    ((null? item-list) 0)
    ((> item (car item-list)) (+ (inversion-helper item (cdr item-list)) 1))
    (#t (inversion-helper item (cdr item-list)))))

(define (get_inversions item-list)
  (cond
    ((null? item-list) 0)
    ((list? item-list) (+ (get_inversions (cdr item-list)) (inversion-helper (car item-list) (cdr item-list))))))