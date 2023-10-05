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

;(resolve_env 'b var_env)
;(call (function (x) x) 1)
(define parsed
  (parser '(call (function (x) x) 1))
  )
;(print parsed)
(processor parsed var_env)