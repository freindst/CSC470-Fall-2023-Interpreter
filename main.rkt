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
;(parsed '(== a b)) -> (bool-exp == a b)
;(processor (bool-exp == 2 1)) -> #f
(define parsed
  (parser '(+ 2 a))
  )
parsed
;(processor parsed var_env)