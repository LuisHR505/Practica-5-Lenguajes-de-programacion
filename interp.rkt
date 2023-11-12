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
    [op (f args) (interp (parse (apply f (transform (map (lambda (e) (interp e env)) args)))) env)]
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
    [rec (bindings body) (let* ([environment (create-mt-env bindings env)]) (interp body environment))]))

(define (create-mt-env bindings env)
  (if (empty? bindings)
      env
      (type-case RCFSBAE (binding-value (car bindings))
        [fun (p a)
             (create-mt-env (cdr bindings)
                            (cyclically-bind-and-interp
                             (binding-id (car bindings))
                             (binding-value (car bindings))
                             env))]
        [else
         (create-mt-env
          (cdr bindings)
          (cons-env (binding-id (car bindings))
                    (interp (binding-value (car bindings)) env) env))])))


(define (cyclically-bind-and-interp id value env)
  (let* ([container (box (num-v 1729))]
         [enviroment (rec-cons-env id container env)]
         [val (interp value enviroment)])
    (begin
      (set-box! container val)
      enviroment)))


(define (transform args)
  (if (empty? args)
      '()
      (type-case RCFSBAE-Val (car args)
                 [num-v (n) (cons n (transform (cdr args)))]
                 [bool-v (b) (cons b (transform (cdr args)))]
                 [string-v (s) (cons s (transform (cdr args)))]
                 [closure-v (args body env) (error 'interp "transform invalido")])))

;; closure-params :: symbol x args x Env -> Env
(define (closure-params vars args env)
  (if (equal? (length vars) (length args))
      (if (empty? vars)
          env
          (cons-env (car vars) (interp (car args) env) (closure-params (cdr vars) (cdr args) env)))
      (error 'interp "Numero de argumentos y parametros distinto")))

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