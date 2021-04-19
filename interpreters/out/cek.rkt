#lang racket

(require "../lib/idl.rkt")

; begin interpreter

(def-data Term
  String
  {Abs String Term}
  {App Term Term})

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
    ({Abs x body}
      (continue1 cont (fun (v cont1) (eval (extend env x v) body cont1))))
    ({App fn arg} (eval env fn {App1 arg cont env}))))

(def continue1 (fn1 var3)
  (match fn1
    ({App2 cont var2} (var2 var3 cont))
    ({App1 arg cont env} (eval env arg {App2 cont var3}))
    ({Halt } var3)))

(def main ([Term term]) (eval init term {Halt }))

; end interpreter