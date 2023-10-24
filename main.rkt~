#lang racket
(require "Utility.rkt")
(require "Parser.rkt")
(require "Processor.rkt")

(define var_env
  '(;environment
   (;global variable scope
    (a 1) (b 2) (c 3)
    )
   )
  )

(define parsed '(call (function (x y) (* x y)) (5 c)))
(parser parsed)
;parsed-> (app-exp (func-exp ((var-exp x) (var-exp y)) (math-exp * (var-exp x) (var-exp y)))
;((num-exp 5) (var-exp c)))
;(define parsed
;  (parser '(call (function(x)(ask (== a 1) (+ x 1) (- x 1))) (2)))
;  )
;parsed
(processor (parser parsed) var_env)

