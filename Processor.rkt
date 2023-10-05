#lang racket
(require "Utility.rkt")

;(var-exp a)
;(processor (var-exp a)) -> (resolve a variable_env) -> 1
(define process_var_exp
  (lambda
      (parsedCode env)
    (resolve_env (cadr parsedCode) env)
    )
  )

;(num-exp 1) -> 1
(define process_num_exp
  (lambda
      (parsedCode env)
    (cadr parsedCode)
    )
  )
    

;(processor (app-exp (func-exp ((var-exp x)) (var-exp x)) (var-exp a)))
;function func(x){...}; func(1)
(define process_app_exp
  (lambda
      (parsedCode env)
    (let
        (
         (local_env
          (push_var_to_env
           (cadr (car (cadr (cadr parsedCode))))
           (processor (caddr parsedCode) env)
           env)
          )
         )
      (processor (caddr (cadr parsedCode)) local_env)
      )
    )
  )


(define processor
  (lambda
      (parsedCode env)
    (cond
      ;when parsed Code is empty
      ((null? parsedCode) (displayln "Error: Processor receives an illegal parsed code"))
      ;when parsed code is a var expression
      ((eq? 'var-exp (car parsedCode))
       (process_var_exp parsedCode env))
      ;when parsed code is a app expression
      ((eq? 'app-exp (car parsedCode))
       (process_app_exp parsedCode env))
      ;when parsed code is a numeric expression
      ((eq? 'num-exp (car parsedCode))
       (process_num_exp parsedCode env))
      ;when parsed code is a boolean expression
      ;....
      ;otherwise
      (else #f)
      )
    )
  )

(provide (all-defined-out))