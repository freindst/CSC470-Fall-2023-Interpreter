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
    

;(processor (app-exp (func-exp (list-exp (var-exp x) (var-exp y)) (var-exp x))
;(list-exp (var-exp a) (num-exp 5)))
;(list-exp (var-exp x) (var-exp y)) -> (x y)
;(list-exp (var-exp a) (num-exp 5)) -> (process (var-exp a)) (process (num-exp 5)
;function func(x){...}; func(1)
(define process_app_exp
  (lambda
      (parsedCode env)
    (let*
        (
         ;structure of env ((.pairs.)(.pairs.)...(.pairs..)(global-variable-scope))
         (global_env (trim_to_global_scope env));this is the environment only has the global scope;
;local_env only has the variable-value paris of the argument list and the global variable scope
         (local_env
          (push_vars_to_env
           (map (lambda (arg) (cadr arg)) (cdr (car (cadr (cadr parsedCode)))))
           (map (lambda (val-exp) (processor val-exp env))
                (cdr (caddr parsedCode)))
           global_env)
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
     (else (error-output "Illegal boolean expression"))
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
     (else (error-output "Illegal math expression"))
      )
   )
  )

;(let-exp
;(list-exp ((var-exp d) (num-exp 10)) ((var-exp f) (num-exp 20)))
;(math-exp + (var-exp d) (math-exp + (var-exp f) (var-exp x))
(define process_let_exp
  (lambda (parsedCode env)
    (let*
        ((varname_value_list;varname_value_list = ((d 10) (e 20))
          (map (lambda (pair)
                 (list (cadr (car pair)) (processor (cadr pair) env)))
               (cdr (cadr parsedCode))))
         ;((d 10) (e 20)) + (((x 5)) ((a 1) (b 2) (c 3)))
         ;(((d 10) (e 20) (x 5)) ((a 1) (b 2) (c 3)))
         (let_local_env (cons (append varname_value_list (car env)) (cdr env))))
      (processor (caddr parsedCode) let_local_env)
     )
    )
  )

(define processor
  (lambda
      (parsedCode env)
    (cond
      ;when parsed Code is empty
      ((null? parsedCode) (error-output "Processor receives an illegal parsed code"))
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
      ;when parsed code is a let expression
      ((eq? 'let-exp (car parsedCode))
       (process_let_exp parsedCode env))
      ;when parsed code is an output expression
      ((eq? 'output-exp (car parsedCode))
       (displayln (string-append "***output***: "(number->string (processor (cadr parsedCode) env)))))
      ;when parsed code is a block expression
      ((eq? 'block-exp (car parsedCode))
       (pick_first_non_void_from_list
        (map (lambda (code) (processor code env)) (cdr parsedCode))))
      ;....
      ;otherwise
      (else (error-output "Processor failed to produce result."))
      )
    )
  )


(provide (all-defined-out))