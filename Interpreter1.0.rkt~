#lang racket
(define variable_env '(((a 1)(b 2)(c 33)(d "string")(e a))))
(define resolve_scope (lambda (variable_name scope)
                  (cond
                    ((null? scope) null)
                    ((eq? (car (car scope)) variable_name) (car (cdr (car scope))))
                    (else (resolve_scope variable_name (cdr scope)))
                    )
                  )
  )

(define resolve (lambda (variable_name)
                  (if
                    (null? variable_env)
                    (print "Error: Variable not found in the environment.")
                    (if (null? (resolve_scope variable_name (car variable_env)))
                        (resolve variable_name (cdr variable_env))
                        (resolve_scope variable_name (car variable_env))
                        )
                    )
                  )
  )

(define append_var (lambda (variable_name variable_value)
                     (set! variable_env
                           (cons
                            (cons (list variable_name variable_value) (car variable_env))
                            (cdr variable_env)
                            )
                           )
                     )
  )
(define push_scope (lambda ()
                     (
                      print ""
                      ;return cons)
                     )
  )
  )

(define pop_scope (lambda ()
                    (print ""
                     ;return cdr)
                    )
  )
  )

;(parser 'a) -> (var-exp a)
;(parser '(function (x) x)) -> (func-exp ((var-exp x)) (var-exp x))
;(parser '(call (function (x) x) a)) -> (app-exp (func-exp ((var-exp x)) (var-exp x)) (var-exp a))
(define parser (lambda (statement)
                 (cond
                  ((symbol? statement) (list 'var-exp statement)) ;this is a variable expression
                  ((and
                   (list? statement)
                   (eq? 'function (car statement))
                   (eq? (length statement) 3)
                   )
                   (list 'func-exp (list (parser (car (cadr statement)))) (parser (caddr statement)))
                   )
                   ;this is a function expression
                   (
                    (and
                     (list? statement)
                     (eq? 'call (car statement))
                     (eq? (length statement) 3)
                     )
                    (list 'app-exp (parser (cadr statement)) (parser (caddr statement)))
                    );this is an app epxression
                  (print "Parsing failed. Unknown statement.")
                 
                  )

                 )
  )

(parser '(call (function (x) x) a))

;(processor (var-exp a)) -> (resolve a variable_env) -> 1
(define processor (lambda (parse variable_env)
                    (cond
                      ((null? parse) (print "Processing failed. Bad parsed syntax."))
                      ((eq? (car parse) 'var-exp) (resolve (cadr parse)))
                      ((eq? (car parse) 'app-exp) (print "more to do"))
                      (else "Processing failed. Unknown issue.")
                      )
                    )
  )

(processor (parser 'a) variable_env)

(define executor (lambda (code)
                   (processor (parser code) variable_env)
                   )
  )


;(executor '(call (function (x) z) a))
(append_var 'hunter 20)
(print variable_env)
(resolve 'hunter)