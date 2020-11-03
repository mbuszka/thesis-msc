#lang racket

(require "../lib/idl.rkt")

; begin interpreter

(def-data Term
  String
  {Abs String Term}
  {App Term Term})

(def-struct {Fun body env x})
(def-struct {App2 cont var2})
(def-struct {App1 arg cont env})
(def-struct {Halt })
(def init (x) (error "empty environment"))

(def extend (env y v)
  (fun (x)
    (match (eq? x y)
      (#t v)
      (#f (env x)))))

(def eval (env term cont)
  (match term
    ([String x] (continue1 cont (env x)))
    ({Abs x body} (continue1 cont {Fun body env x}))
    ({App fn arg} (eval env fn {App1 arg cont env}))))

(def apply (fn1 v cont1)
  (match fn1 ({Fun body env x} (eval (extend env x v) body cont1))))

(def continue1 (fn2 var3)
  (match fn2
    ({App2 cont var2} (apply var2 var3 cont))
    ({App1 arg cont env} (eval env arg {App2 cont var3}))
    ({Halt } var3)))

(def main ([Term term]) (eval init term {Halt }))

; end interpreter