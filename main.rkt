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

(define code '(block (class Person
   (fields (name))
   (constructor (name) ((this name) name) )
   (methods((tell () (out (this name)))))
))) ; we would like to repeat print a for 4 times
;parse-> (each-exp (assign-exp a (num-exp 0))
; (each-body-exp (bool-exp < (num-exp 5) (var-exp a)) (assign-exp a (math-exp + (var-exp a) (num-exp 1)))
; (each-list-exp (output-exp (var-exp a)))))
(define parsed (parser code))
parsed
(processor parsed var_env)

