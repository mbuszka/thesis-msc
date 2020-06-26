#lang racket

(require "../lib/idl.rkt")
(require
  (for-syntax syntax/parse
              racket/syntax))

; begin interpreter

(def-data Term
  String
  Integer
  {Lam String Term}
  {App Term Term}
  {Add Term Term}
  {Raise Integer}
  {Try Term String Term})

(def-data Res
  {Err Integer}
  {Ok Any})

(def-struct {Fun body env x})
(def-struct {Ok1 cont f})
(def-struct {App1 arg cont env})
(def-struct {Ok2 cont n})
(def-struct {Add1 cont env m})
(def-struct {Try1 cont env handle x})
(def-struct {Extend env k v})
(def-struct {Halt })
(def-struct {Init })
(def eval (env [Term term] cont)
  (match term
    ([String x] (continue1 cont {Ok (lookup env x)}))
    ([Integer n] (continue1 cont {Ok n}))
    ({Lam x body} (continue1 cont {Ok {Fun body env x}}))
    ({App fn arg} (eval env fn {App1 arg cont env}))
    ({Add n m} (eval env n {Add1 cont env m}))
    ({Raise n} (continue1 cont {Err n}))
    ({Try t x handle} (eval env t {Try1 cont env handle x}))))

(def extend (env k v) {Extend env k v})

(def init (x) (error "empty environment"))

(def apply (fn1 v cont1)
  (match fn1 ({Fun body env x} (eval (extend env x v) body cont1))))

(def continue1 (fn2 var3)
  (match fn2
    ({Ok1 cont f}
      (match var3
        ({Err e} (continue1 cont {Err e}))
        ({Ok v} (apply f v cont))))
    ({App1 arg cont env}
      (match var3
        ({Err e} (continue1 cont {Err e}))
        ({Ok f} (eval env arg {Ok1 cont f}))))
    ({Ok2 cont n}
      (match var3
        ({Err e} (continue1 cont {Err e}))
        ({Ok m} (continue1 cont {Ok (+ n m)}))))
    ({Add1 cont env m}
      (match var3
        ({Err e} (continue1 cont {Err e}))
        ({Ok n} (eval env m {Ok2 cont n}))))
    ({Try1 cont env handle x}
      (match var3
        ({Ok v} (continue1 cont {Ok v}))
        ({Err e} (eval (extend env x e) handle cont))))
    ({Halt } var3)))

(def lookup (fn3 x)
  (match fn3
    ({Extend env k v}
      (match (eq? x k)
        (#t v)
        (#f (lookup env x))))
    ({Init } (init x))))

(def main ([Term term]) (eval {Init } term {Halt }))

; end interpreter

(module+ test
  (require rackunit)
  (check-equal? (main 42) {Ok 42})
  (check-equal? (main (App (Lam "x" "x") 42)) {Ok 42})
  
  (define-syntax (lam* stx)
    (syntax-parse stx
      [(_ (v) body) #'(Lam v body)]
      [(_ (v vs ...+) body) #'(Lam v (lam* (vs ...) body))]))

  (define-syntax (app* stx)
    (syntax-parse stx
      [(_ f v) #'(App f v)]
      [(_ f v vs ...+) #'(app* (App f v) vs ...)]))

  (let*
    ([zero (lam* ("s" "z") "z")]
     [succ (lam* ("n" "s" "z") {App "s" (app* "n" "s" "z")})]
     [two {App succ {App succ zero}}]
     [plus (lam*  ("n" "m" "s" "z") (app* "n" "s" (app* "m" "s" "z")))]
     [pgm (app* plus two two {Lam "n" {Add "n" 1}} 0)]
     [pgm1 {Try {Add 1 {Raise 17}} "x" {Add 25 "x"}}])
    (check-equal? (main pgm) {Ok 4})
    (check-equal? (main pgm1) {Ok 42}))
)
 