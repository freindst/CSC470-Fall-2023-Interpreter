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
   (constructor (name) (((this name) name)) )
   (methods((tell () (out (this name)))))
)
                     (initiate p Person ("Tim";(math + 1 2))
                     ))))
;1 done with store the definitions of a class with 'class keyword expression
;2 create new instance of the class using the constructor
(define parsed (parser code))
parsed
;(processor parsed var_env)

