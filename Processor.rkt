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

;(bool-exp == (var-exp a) (num-exp 1))
(define process_bool_exp
  (lambda
   (parsedCode env)
   (cond
     ((eq? '> (cadr parsedCode))
      (> (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
     ((eq? '< (cadr parsedCode))
      (< (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
     ((eq? '>= (cadr parsedCode))
      (>= (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
     ((eq? '<= (cadr parsedCode))
      (<= (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
     ((eq? '== (cadr parsedCode))
      (eq? (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
     ((eq? '&& (cadr parsedCode))
      (and (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
     ((eq? '|| (cadr parsedCode))
      (or (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
     ((eq? '! (cadr parsedCode))
      (not (processor (caddr parsedCode) env)))
     ((eq? '!= (cadr parsedCode))
      (not (eq? (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env))))
     (else (println "Error: illegal boolean expression"))
      )
   )
  )

(define process_ask_exp
  (lambda
   (parsedCode env)
   (if
    (processor (cadr parsedCode) env)
    (processor (caddr parsedCode) env)
    (processor (cadddr parsedCode) env))
   )
  )

;process math ('+ '- '* '/ '// '%)
(define process_math_exp
  (lambda
   (parsedCode env)
   (cond
     ((eq? '+ (cadr parsedCode))
      (+ (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
     ((eq? '- (cadr parsedCode))
      (- (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
     ((eq? '* (cadr parsedCode))
      (* (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
     ((eq? '/ (cadr parsedCode));integer division 5/2 = 2
      (quotient (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
     ((eq? '// (cadr parsedCode));float division, give you mix number 5/2 = 2 and 1/2
      (/ (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
     ((eq? '% (cadr parsedCode))
      (modulo (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
     (else (println "Error: illegal math expression"))
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
      ((eq? 'bool-exp (car parsedCode))
       (process_bool_exp parsedCode env))
      ;when parsed code is a ask expression
      ((eq? 'ask-exp (car parsedCode))
       (process_ask_exp parsedCode env))
      ;when parsed code is a math expression
      ((eq? 'math-exp (car parsedCode))
       (process_math_exp parsedCode env))
      ;....
      ;otherwise
      (else #f)
      )
    )
  )

(provide (all-defined-out))