#lang racket
(require "Utility.rkt")
;(parser 'a) -> (var-exp a)
;(parser '(function (x) x)) -> (func-exp ((var-exp x)) (var-exp x))
;(parser '(call (function (x) x) a)) -> (app-exp (func-exp ((var-exp x)) (var-exp x)) (var-exp a))
(define parser
  (lambda (statement)
    (cond
      ((symbol? statement) (list 'var-exp statement)) ;this is a variable expression
      ((number? statement) (list 'num-exp statement))
      ;bool: (operator op1 op2) -> (bool-exp operator (exp op1) (exp op2))
      ((and
        (list? statement)
        (check_bool_op (car statement))
        (eq? (length statement) 3)
       )
       (list 'bool-exp (car statement) (parser (cadr statement)) (parser (caddr statement)))
       )
      ((and
        (list? statement)
        (eq? '! (car statement))
        (eq? (length statement) 2)
        )
       (list 'bool-exp (car statement) parser (cadr statement))
       )
      ;math expression: (operator op1 op2) -> (math-exp operator (exp op1) (exp op2))
      ((and
        (list? statement)
        (check_math_op (car statement))
        (eq? (length statement) 3)
        )
        (list
         'math-exp
         (car statement)
         (parser (cadr statement))
         (parser (caddr statement)))
        )
      ((and
        (list? statement)
        (eq? 'function (car statement))
        (eq? (length statement) 3)
        )
       (list 'func-exp (list (parser (car (cadr statement)))) (parser (caddr statement)))
       )
      ;this is a function expression
      ;(function (arg) body)
      ((and
        (list? statement)
        (eq? 'call (car statement))
        (eq? (length statement) 3)
        )
       (list 'app-exp (parser (cadr statement)) (parser (caddr statement)))
       );this is an app epxression
      ((and
        (list? statement)
        (eq? 'ask (car statement))
        (eq? (length statement) 4))
        (list
         'ask-exp
         (parser (cadr statement))
         (parser (caddr statement))
         (parser (cadddr statement))
         )
        );this is an ask expression
      (else
       (print "Parsing failed. Unknown statement."))
      )
    )
  )

(define check_bool_op
  (lambda (op)
    (cond
      ((eq? op '>) #t)
      ((eq? op '<) #t)
      ((eq? op '>=) #t)
      ((eq? op '<=) #t)
      ((eq? op '==) #t)
      ((eq? op '&&) #t)
      ((eq? op '||) #t)
      ((eq? op '!) #t)
      ((eq? op '!=) #t)
      (else #f)
      )
    )
  )

(define check_math_op
  (lambda (op)
    (is_in_list (list '+ '- '* '/ '// '%) op)
    )
  )

    
(provide (all-defined-out))