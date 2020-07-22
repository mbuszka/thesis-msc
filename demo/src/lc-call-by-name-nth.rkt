#lang racket

(require "../lib/idl.rkt")

; begin interpreter

(def-data Term
  Integer
  {App Term Term}
  {Abs Term}
  {Unit})

(def cons #:atomic (val env)
  (fun #:atomic #:name Cons #:apply nth (n)
    (match n
      (0 val)
      (_ (env (- n 1))))))

(def eval (expr env)
  (match expr
    ([Integer n] ((env n)))
    ({App f x} ((eval f env) (fun #:name Thunk #:apply force () (eval x env))))
    ({Abs body} (fun #:name Closure (x) (eval body (cons x env))))
    ({Unit} {Unit})
  ))

(def main ([Term term])
  (eval term (fun #:atomic #:name Nil (n) (error "empty env"))))

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