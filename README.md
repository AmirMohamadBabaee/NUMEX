# NUMEX
Number-Expression Programming Language. Final Project of Programming Languages Course at CE@AUT.

# Sample Code
Fibonacci Series Generator Implementation in NUMEX:
```racket
;; NUMEX Fibonacci Series Generator Implementation
(define fibb
  (lam "fibb" "n"
       (cnd (orelse (iseq (var "n") (num 0)) (iseq (var "n") (num 1)))
            (num 1)
            (plus (apply (var "fibb") (minus (var "n") (num 1))) (apply (var "fibb") (minus (var "n") (num 2)))))))
```
```console
> (eval-exp (apply fibb (num 0)))
(num 1)
> (eval-exp (apply fibb (num 1)))
(num 1)
> (eval-exp (apply fibb (num 2)))
(num 2)
> (eval-exp (apply fibb (num 3)))
(num 3)
> (eval-exp (apply fibb (num 4)))
(num 5)
> (eval-exp (apply fibb (num 5)))
(num 8)
> (eval-exp (apply fibb (num 6)))
(num 13)
> (eval-exp (apply fibb (num 7)))
(num 21)
```
