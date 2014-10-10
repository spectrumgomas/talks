#lang plai-typed

;; interp1 written in CPS style
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
  [closV (f : (Value (Value -> Value) -> Value))])

(define (num-zero? [v : Value]) : boolean
  (type-case Value v
    [numV (n) (= n 0)]
    [closV (f) #f]))

;; interpreter
(define (interp [expr : ExprC] [env : Env] [k : (Value -> Value)]) : Value
  (type-case ExprC expr
    [numC (n) (k (numV n))]
    [idC (v) (k (lookup v env))]
    [plusC (l r) (interp l env 
                         (λ(lv)
                           (interp r env
                                   (λ(rv)
                                     (k (num+ lv rv))))))]
    [if0 (p t f) (interp p env
                         (λ(testv)
                           (if (num-zero? testv)
                               (interp t env k)
                               (interp f env k))))]
    [lamC (a b) (k (closV (λ(arg-val dyn-k)
                            (interp b (extend-env (bind a arg-val) env)
                                    dyn-k))))]
    [appC (f a) (interp f env
                        (λ(fv)
                          (interp a env
                                  (λ(argv)
                                    ((closV-f fv) argv k)))))]))
