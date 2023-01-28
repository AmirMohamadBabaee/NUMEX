#lang racket

(define env1 (list (cons "A" 3) (cons "B" 4) (cons "C" 4)))
(define env2 (list (cons "B" 1) (cons "C" 4) (cons "D" 1)))

(define (name p)
  (cond
    ((pair? p) (car p))
    ((null? p) null)))

(define (value p)
  (cond
    ((pair? p) (cdr p))
    ((null? p) null)))

(define (has-name p env)
  (cond
    ((null? env) null)
    ((and (eq? (name p) (name (car env)))
          (not (eq? (value p) (value (car env)))))
     (name p))
    (#t
     (has-name p (cdr env)))))

(define (diff_envs e1 e2)
  (cond
    ((null? e1) (list))
    ((list? e1) (cond
                  ((not (null? (has-name (car e1) e2))) (cons (name (car e1)) (diff_envs (cdr e1) e2)))
                  (#t (diff_envs (cdr e1) e2))))))
