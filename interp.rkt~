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
(define (interp expr env) (error 'interp "Sin implementar"))

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