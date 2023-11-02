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

(define code '(when (< a 5) (let ((a (+ a 1))) (out a))))
;parse->(when-exp (bool-exp < a 5)(block-exp (output-exp (var-exp a)) (let-exp ...)

;the challenge is that we need to look fo a in the environment, and update a in its environment
(define parsed (parser code))
parsed
;(processor parsed var_env)
