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

(define code '(while (<  a 10) (+ a 1))); a very simple expression: (while-exp (boolean-exp < a 10) (math-exp + a 1))

;the challenge is that we need to look fo a in the environment, and update a in its environment
(define parsed (parser code))
parsed
;(processor parsed var_env)
