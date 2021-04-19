#lang racket

(require "../lib/idl.rkt")

; begin interpreter

(def-data Term
  String
  {Abs String Term}
  {App Term Term})

(def init #:atomic #:no-defun (x) (error "empty environment"))

(def extend #:atomic (env y v)
  (fun #:atomic #:no-defun (x) (match (eq? x y)
    (#t v)
    (#f (env x)))))

(def eval (env term)
  (match term
    ([String x] (env x))
    ({Abs x body} (fun #:name Fun #:apply apply #:no-defun (v) (eval (extend env x v) body)))
    ({App fn arg} ((eval env fn) (eval env arg)))))
        
(def main ([Term term]) (eval init term))

; end interpreter