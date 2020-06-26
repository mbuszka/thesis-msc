#lang racket

(require "../lib/idl.rkt")

; begin interpreter

(def-data Arith
  Integer
  String
  {Add Arith Arith})

(def-data Bool {Eq Arith Arith})

(def-data Cmd
  {Skip }
  {Assign String Arith}
  {If Bool Cmd Cmd}
  {Seq Cmd Cmd}
  {While Bool Cmd})

(def-struct {Seq1 cmd2 cont})
(def-struct {While1 cmd cond cont})
(def-struct {Halt })
(def init-state (var) 0)

(def update-state (tgt val state)
  (fun (var)
    (match (eq? tgt var)
      (#t val)
      (#f (state var)))))

(def aval (state aexpr)
  (match aexpr
    ([Integer n] n)
    ([String var] (state var))
    ({Add aexpr1 aexpr2} (+ (aval state aexpr1) (aval state aexpr2)))))

(def bval (state bexpr)
  (match bexpr
    ({Eq aexpr1 aexpr2} (eq? (aval state aexpr1) (aval state aexpr2)))))

(def eval (state cmd cont)
  (match cmd
    ({Skip } (continue cont state))
    ({Assign var aexpr}
      (continue cont (update-state var (aval state aexpr) state)))
    ({If cond then else}
      (match (bval state cond)
        (#t (eval state then cont))
        (#f (eval state else cont))))
    ({Seq cmd1 cmd2} (eval state cmd1 {Seq1 cmd2 cont}))
    ({While cond cmd}
      (match (bval state cond)
        (#t (eval state cmd {While1 cmd cond cont}))
        (#f (continue cont state))))))

(def continue (fn state)
  (match fn
    ({Seq1 cmd2 cont} (eval state cmd2 cont))
    ({While1 cmd cond cont} (eval state {While cond cmd} cont))
    ({Halt } state)))

(def main ([Cmd cmd]) (eval init-state cmd {Halt }))

; end interpreter

(module+ test
  (require (for-syntax syntax/parse))
  (require rackunit)
  
  (define-syntax (do stx)
    (syntax-parse stx
      [(_ cmd) #'cmd]
      [(_ c cmd ...+) #'(Seq c (do cmd ...))]))

  (let*
    ([pgm (do {Skip})]
     [state (main pgm)])
    (check-equal? (state "x") 0))

  (let*
    ([pgm 
      (do 
        {Assign "x" 42}
        {Assign "y" {Add "x" "y"}}
        {If {Eq "x" "y"}
          {Assign "z" 1}
          {Skip}})]
     [state (main pgm)])
    (check-equal? (state "z") 1))
)
