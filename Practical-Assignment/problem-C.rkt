#lang racket

(struct balance (left right str) #:transparent)

(define (increase-left b)
  (cond
    ((balance? b) (balance (+ (balance-left b) 1) (balance-right b) (string-append (balance-str b) "(")))
    (#t (error "some error"))))

(define (increase-right b)
  (cond
    ((balance? b) (balance (balance-left b) (+ (balance-right b) 1) (string-append (balance-str b) ")")))
    (#t (error "some error"))))

(define (generate_all n)

  (local
      [(define (fn-bal-check generated_list valid_list)
         (if (empty? generated_list)
             valid_list
             (fn-bal-generate (car generated_list) (cdr generated_list) valid_list)))

       (define (fn-bal-generate bal generated_list valid_list)
         (cond [(or (> (balance-left bal) n)
                    (< (balance-left bal) (balance-right bal)))
                (fn-bal-check generated_list valid_list)]

               [(= (string-length (balance-str bal)) (* 2 n))
                (fn-bal-check generated_list (cons (balance-str bal) valid_list))]

               [#t
                (fn-bal-check (append (list (balance (+ (balance-left bal) 1) (balance-right bal) (string-append (balance-str bal) "(")))
                                    (list (balance (balance-left bal) (+ (balance-right bal) 1) (string-append (balance-str bal) ")")))
                                    generated_list)
                            valid_list)]))]

    (fn-bal-check (list (balance 0 0 "")) (list))))