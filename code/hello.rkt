#lang racket

(define (factorial n)
  (cond [(= n 0) 1]
        [else (* n (factorial (sub1 n)))]))

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define (fib n)
  (cond [(= n 0) 1]
        [(= n 1) 1]
        [else (+ (fib (- n 1))
                 (fib (- n 2)))]))

;; closures
(define (cons x y)
  (define (dispatch m)
    (cond [(= m 0) x]
          [(= m 1) y]
          [else (error "argument is not 0 or 1 -- cons")]))
  dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))

;; lambda
(define (plus4 x) (+ x 4))

;; is equivalent to
(define new-plus4 (lambda (x) (+ x 4)))

;; let binding
(define (make-rat n d)
  (let ([g (gcd n d)])
    (cons (/ n g) (/ d g))))
