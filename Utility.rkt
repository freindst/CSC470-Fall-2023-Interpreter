#lang racket
;resolve variable from scope
;(resolve_env a var_env) -> 1
(define resolve_env
  (lambda
      (varname env)
    (cond
      ((null? env) (print "Error: Variable not found."))
      ((eq? #f (resolve_scope varname (car env)))
       (resolve_scope varname (cdr env)))
      (else (resolve_scope varname (car env)))
     )
    )
  )


;resolve variable from environment
;(resolve_scope a (car var_env)) -> 1
(define resolve_scope
  (lambda
      (varname scope)
    (cond
      ((null? scope) #f)
      ((eq? varname (car (car scope)))
       (car (cdr (car scope))))
      (else
       (resolve_scope varname (cdr scope)))
      )
    )
  )

;create a new scope with a variable-value pair and push the scope to environment
;(push_var_to_env d 4 env) -> (((d 4)) ...env)
(define push_var_to_env
  (lambda (varName varValue env)
    (cons
     (list (list varName varValue))
     env
     )
    )
  )

(provide (all-defined-out))