#lang plai

(require "grammars.rkt")
(require "parser.rkt")

(define (boxed-RCFSBAE-Val? b)
  (and (box? b) (RCFSBAE-Val? (unbox b))))

(define-type RCFSBAE-Val
  [num-v (n number?)]
  [bool-v (b boolean?)]
  [string-v (s string?)]
  [closure-v (args (listof symbol?)) (body RCFSBAE?) (env Env?)])

(define-type Env
  [mt-env]
  [cons-env (id symbol?) (value RCFSBAE-Val?) (rest-env Env?)]
  [rec-cons-env (id symbol?) (value boxed-RCFSBAE-Val?) (rest-env Env?)])

;; RCFSBAE x Env -> RCFSBAE-Val
(define (interp expr env)
  (type-case RCFSBAE expr
    [num (n) (num-v n)]
    [id (i) (lookup id env)]
    [bool (b) (bool-v b)]
    [strinG (s) (string-v s)]
    [op (f args) (interp (parse (apply f (greedy-eval (map (lambda (e) (interp e env)) args)))) mt-env)]
    [fun (param body) (closure-v param body env)]
    [app (fun-e args)(let ([fun-val (interp fun-e env)])
                         (interp (closure-v-body fun-val)
                                 (closure-params (closure-v-args fun-val) args (closure-v-env fun-val))))]
    [iF (test-e then-e else-e)
        (type-case RCFSBAE-Val (interp test-e env)
          [bool-v (b)
                  (if b
                      (interp then-e env)
                      (interp else-e env))]
          [else (error 'interp "La condicion del if debe ser un booleano")])]
    [rec (bindings body) (interp (rec->aux bindings body env))]))

(define (rec->aux bindings body env)
  (cond
    [(empty? bindings) body]
    [else
     (type-case Binding bindings
       [binding (id val)
                (let* ([contenedor (box (num-v 1729))] ; ; Paso 1
                       [ambiente (cons-env id contenedor env)]
                       [valor (interp val ambiente)])
                  (begin
                    (set-box! contenedor valor) ; ; Paso 2
                    ambiente
                    (rec->aux (cdr bindings) body env))
                  )])]))

;; closure-params :: symbol x args x Env -> Env
(define (closure-params vars args env)
  (if (equal? (length vars) (length args))
      (if (empty? vars)
          env
          (cons-env (car vars) (interp (car args) env) (closure-params (cdr vars) (cdr args) env)))
      (error 'interp "Numero de argumentos y parametros distinto")))

;;greedy-eval :: CFSBAE-Val -> Val
(define (greedy-eval args)
  (if (empty? args)
      '()
      (type-case RCFSBAE-Val (car args)
        [num-v (n) (cons n (greedy-eval (cdr args)))]
        [bool-v (b) (cons b (greedy-eval (cdr args)))]
        [string-v (s) (cons s (greedy-eval (cdr args)))]
        [closure-v (args body env) error 'interp "error de sintaxis"] )))

;; symbol x Env -> RCFSBAE-Val

(define (lookup sub-id env)
  (type-case Env env
    [mt-env ( ) (error 'lookup (string-append "interp: Variable libre " (symbol->string sub-id)))]
    
    [cons-env (id value rest)(if (equal? id sub-id)
                                 value
                                 (lookup sub-id rest)
                                 )]
    [rec-cons-env (id box rest) (if (equal? id sub-id)
                                 (unbox box)
                                 (lookup sub-id rest)
                                 )]
    ))

;; ambiente de prueba
;;(rec-cons-env 'x (box (num-v 10)) (mt-env))