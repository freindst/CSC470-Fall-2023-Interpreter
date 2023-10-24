#lang racket
;resolve variable from scope
;(resolve_env a var_env) -> 1
(define resolve_env
  (lambda
      (varname env)
    (cond
      ((null? env) (print "Error: Variable not found."))
      ((eq? #f (resolve_scope varname (car env)))
        (resolve_env varname (cdr env)))
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

;(push_vars_to_env (x y z) (1 2 3)) -> (((x 1) (y 2) (z 3)) env)
(define push_vars_to_env
  (lambda (list_var list_val env)
  (cons (pair_helper list_var list_val) env)
  )
  )

;add varable and value paris to the current scope
(define add_vars_to_top_scope
  (lambda (list_var list_val env)
    (cons
     (append (pair_helper list_var list_val) (car env))
     (cdr env)
     )
    )
  )

(define pair_helper
  (lambda (list_var list_val)
    (if (null? list_var)
        '()
        (cons (list (car list_var) (car list_val)) (pair_helper (cdr list_var) (cdr list_val)))
        )
    )
  )
    

(define is_in_list
  (lambda (lst item)
    (cond
      ((null? lst) #f)
      ((eq? (car lst) item) #t)
      (else (is_in_list (cdr lst) item))
      )
    )
  )

(provide (all-defined-out))