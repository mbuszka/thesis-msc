#lang racket

(require "../lib/idl.rkt")

; begin interpreter

(def-data Term
  Integer
  {App Term Term}
  {Abs Term}
  {Unit })

(def-struct {Thunk env x})
(def-struct {Closure body env})
(def-struct {Cons env val})
(def-struct {App1 cont env x})
(def-struct {Nil })
(def-struct {Halt })
(def cons (val env) {Cons env val})

(def eval (expr env cont)
  (match expr
    ([Integer n] (force (nth env n) cont))
    ({App f x} (eval f env {App1 cont env x}))
    ({Abs body} (continue cont {Closure body env}))
    ({Unit } (continue cont {Unit }))))

(def force (fn cont1) (match fn ({Thunk env x} (eval x env cont1))))

(def apply (fn1 x cont2)
  (match fn1 ({Closure body env} (eval body (cons x env) cont2))))

(def nth (fn2 n)
  (match fn2
    ({Cons env val}
      (match n
        (0 val)
        (_ (nth env (- n 1)))))
    ({Nil } (error "empty env"))))

(def continue (fn3 var3)
  (match fn3
    ({App1 cont env x} (apply var3 {Thunk env x} cont))
    ({Halt } var3)))

(def main ([Term term]) (eval term {Nil } {Halt }))

; end interpreter

(module+ test
  (require rackunit)
  (require
    (for-syntax syntax/parse
                racket/syntax))
  (check-equal? (main {Unit}) {Unit})
  (check-equal? (main (App (Abs 0) {Unit})) {Unit})
  
  (define-syntax (app* stx)
    (syntax-parse stx
      [(_ f v) #'(App f v)]
      [(_ f v vs ...+) #'(app* (App f v) vs ...)]))

  (let*
    ([omega {App {Abs {App 0 0}} {Abs {App 0 0}}}]
     [const {Abs {Abs 1}}])
    (check-equal? (main (app* const {Unit} omega)) {Unit}))
)