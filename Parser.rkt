#lang racket
;(parser 'a) -> (var-exp a)
;(parser '(function (x) x)) -> (func-exp ((var-exp x)) (var-exp x))
;(parser '(call (function (x) x) a)) -> (app-exp (func-exp ((var-exp x)) (var-exp x)) (var-exp a))
(define parser
  (lambda (statement)
    (cond
      ((symbol? statement) (list 'var-exp statement)) ;this is a variable expression
      ((number? statement) (list 'num-exp statement))
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

(provide (all-defined-out))