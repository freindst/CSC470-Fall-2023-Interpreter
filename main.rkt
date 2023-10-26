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

(define code '(call (function (a) (call (function (r) a ) (a))) (5)))
(define parsed (parser code))
(processor parsed var_env)
;correct the answer from 5 to 1
