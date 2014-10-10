#lang plai-typed

(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))

(define mt-env empty)
(define extend-env cons)

;; some utilities
(define (lookup [s : symbol] [env : Env]) : Value
  (cond [(empty? env) (error 'lookup "name not found")]
        [else (cond [(symbol=? s (bind-name (first env))) (bind-val (first env))]
                    [else (lookup s (rest env))])]))

(define (num+ [l : Value] [r : Value])
  (cond [(and (numV? l) (numV? r))
         (numV (+ (numV-n l) (numV-n r)))]
        [else (error 'num+ "invalid argument")]))

;; input type (core language)
(define-type ExprC
  [numC (n : number)]
  [idC (v : symbol)]
  [lamC (arg : symbol) (body : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [appC (fun : ExprC) (arg : ExprC)]
  [if0 (test : ExprC) (truth : ExprC) (falsity : ExprC)])

;; output (value) type
(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])

;; interpreter
(define (interp [expr : ExprC] [env : Env]) : Value
  (type-case ExprC expr
    [numC (n) (numV n)]
    [idC (s) (lookup s env)]
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [if0 (p t e) (if (zero? (interp p env))
                     (interp t env)
                     (interp e env))]
    [lamC (a b) (closV a b env)]
    [appC (f a) (let ([f-value (interp f env)])
                  (interp (closV-body f-value)
                          (extend-env (bind (closV-arg f-value)
                                            (interp a env))
                                      (closV-env f-value))))]))

