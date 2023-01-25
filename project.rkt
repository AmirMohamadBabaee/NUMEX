;; PL Project - Fall 2022
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; Constants and Variables
(struct var     (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct num     (int)    #:transparent)  ;; a constant number, e.g., (num 17)
(struct bool    (bool)   #:transparent)  ;; a constant boolean, e.g., (bool true)

;; num Constant Operations
(struct plus    (e1 e2)  #:transparent)  ;; add two expressions
(struct minus   (e1 e2)  #:transparent)  ;; subtract two expression
(struct mult    (e1 e2)  #:transparent)  ;; multiply two expression
(struct div     (e1 e2)  #:transparent)  ;; divide two expression

;; bool Constant Operations
(struct neg     (e1)     #:transparent)  ;; negation of an expression
(struct andalso (e1 e2)  #:transparent)  ;; logical and of two expression
(struct orelse  (e1 e2)  #:transparent)  ;; logical or of two expression

;; Condition
(struct cnd (e1 e2 e3) #:transparent) ;; result is e2 if e1 is true else the result is e3
(struct iseq (e1 e2)   #:transparent) ;; check equality of two expression
(struct ifnzero (e1 e2 e3) #:transparent) ;; result is e2 if e1 is not zero else the result is e3
(struct ifleq (e1 e2 e3 e4) #:transparent) ;; result is e3 if e1 be less that or equeal e2 else the result is e4

;; Lambda Functions and Applications
(struct lam  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct tlam  (nameopt formal arg-type body) #:transparent) ;; a typed argument, recursive(?) 1-argument function
(struct apply (funexp actual)       #:transparent) ;; function application

;; With Environment
(struct with (s e1 e2) #:transparent)  ;; a let expression where the value of e1 is bound to s in e2

;; Declare NUMEX Nullity
(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then true else false

;; pairs
(struct apair (e1 e2) #:transparent) ;; NUMEX pair of e1 and e2
(struct 1st (e1) #:transparent) ;; first element of a NUMEX pair
(struct 2nd (e1) #:transparent) ;; second element of a NUMEX pair

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env f) #:transparent) 

;; Keys and Records
(struct key  (s e) #:transparent) ;; key holds corresponding value of s which is e
(struct record (k r) #:transparent) ;; record holds several keys
(struct value (s r) #:transparent) ;; value returns corresponding value of s in r

;; Recursive definition
(struct letrec (s1 e1 s2 e2 e3) #:transparent) ;; a letrec expression for recursive definitions

;; Type structures
;; Primitive types are: "int", "bool" and "null"
(struct collection (type) #:transparent) ;; collection of a certain type, e.g., (collection "int")
(struct function (input-type output-type) #:transparent) ;; e.g. (function ("int" int")) means fn f "int" -> "int"

;; Problem 1

(define (racketlist->numexlist xs) (cond
                                     ((null? xs) (munit))
                                     ((pair? xs) (apair (car xs) (racketlist->numexlist (cdr xs))))))
(define (numexlist->racketlist xs) (cond
                                     ((ismunit xs) '())
                                     ((apair? xs) (cons (1st xs) (numexlist->racketlist (2nd xs))))))

;; Problem 2

;; lookup a variable in an environment
;; Complete this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
  	[(list? env) (cond
                       ((eq? (caar env) str) (cdar env))
                       (#t (envlookup (cdr env) str)))]
        [#t (error "TypeValidationError: invalid argument type" env)]
		)
 )

;; Complete more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(plus? e) 
         (let ([v1 (eval-under-env (plus-e1 e) env)]
               [v2 (eval-under-env (plus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (+ (num-int v1) 
                       (num-int v2)))
               (error "NUMEX addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(string? e) e]
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3
;; Complete more cases for other kinds of NUMEX expressions.
;; We will test infer-under-env by calling its helper function, infer-exp.
(define (infer-under-env e env)
  (cond [(var? e) 
         (infer-under-env (envlookup env (var-string e)) env)]

        [(plus? e) 
         (let ([t1 (infer-under-env (plus-e1 e) env)]
               [t2 (infer-under-env (plus-e2 e) env)])
           (if (and (equal? "int" t1)
                    (equal? "int" t2))
               "int"
               (error "NUMEX TYPE ERROR: addition applied to non-integer")))]

        [(num? e)
         (cond
           [(integer? (num-int e)) "int"]
           [#t (error "NUMEX TYPE ERROR: num should be a constant number")])]

        [(bool? e)
         (cond
           [(boolean? (bool-b e)) "bool"]
           [#t (error "NUMEX TYPE ERROR: bool should be #t or #f")])]

        ;; CHANGE add more cases here
        [(string? e) e]
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (infer-exp e)
  (infer-under-env e null))

;; Problem 4

(define (ifmunit e1 e2 e3) "CHANGE")

(define (with* bs e2) "CHANGE")

(define (ifneq e1 e2 e3 e4) "CHANGE")

;; Problem 5

(define numex-filter "CHANGE")

(define numex-all-gt
  (with "filter" numex-filter
        "CHANGE (notice filter is now in NUMEX scope)"))

;; Problem 6

(define type-error-but-evaluates-ok "CHANGE")
(define type-ok-but-evaluates-error "CHANGE")

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
