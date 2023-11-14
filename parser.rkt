#lang plai

(require "grammars.rkt")

;; s-expr -> RCFSBAE
(define (parse s-exp)
  (cond
    [(number? s-exp) (num s-exp)]
    [(boolean? s-exp) (bool s-exp)]
    [(string? s-exp) (strinG s-exp)]
    [(symbol? s-exp) (id s-exp)]
    [(list? s-exp) (parse-ls s-exp)]))

(define (parse-ls s-exp)
  (let ([fst (first s-exp)]
        [rst (rest s-exp)])
    (case fst
      [(fun) (fun (no-rep (first rst) '{}) (parse (second rst)))]
      [(+ - / * min max abd ir < > <= >= = or and )
       (if (empty? rst)
           (error 'parse (string-append "La operación " (symbol->string fst)
                                        " debe ser ejecutada con mas de 0 argumentos."))
           (op (symbol->procedure fst)
                (map parse rst)))]
      [(modulo expt)
       (if (not (= (length rst) 2))
           (error 'parse (string-append "La operación " (symbol->string fst)
                                        " espera 2 argumentos."
                                        " Número de argumentos dados: "
                                        (~a (length rst))
                                        "."))
           (op (symbol->procedure fst)
                (map parse rst)))]
      [(sub1 add1 not zero? num? str? bool? str-length sqrt)
       (if (not (= (length rst) 1))
           (error 'parse (string-append "La operación " (symbol->string fst)
                                        " debe ser ejecutada con 1 argumentos."))
           (op (symbol->procedure fst)
                (map parse rst)))]
      
      [(if) (if (< (length rst) 3)
                (error 'parse "Falta la else-expression.")
                (iF (parse (first rst))
                     (parse (second rst))
                     (parse (third rst))))]
      
      [(rec)   (let* ([vars (car rst)]
                      [bindings (mapBindings vars '{})])
               (rec bindings (parse (second rst))))] 
      
      [else (app (parse fst) (map parse rst))]
)))

;(define (parse-condition cond)
;  (condition (parse (first cond))
;             (parse (second cond))))

(define (symbol->procedure sym)
  (case sym
    ;; Aquí van los casos especiales que no son evaluados directamente de Racket
    [(string?) (string)]
    [(str?) (eval string?)]
    [(str-length) (eval string-length)]
    [(or) (eval oR)]
    [(and) (eval anD)]
    [(bool?) (eval boolean?)]
    [(num?) (eval number?)]
    [else (eval sym)]
  ))

(define (anD . args)
   (anD1 args))

(define (anD1 listaB)
  (if(empty? listaB)
     #t
     (and (car listaB) (anD1 (cdr listaB)))
     ))


(define (oR . args)
   (oR1 args))


(define (oR1 listaB)
  (if(empty? listaB)
     #f
     (or (car listaB) (oR1 (cdr listaB)))
     ))


(define (list-to-binding ls)
  (if (empty? ls)
      (error 'list-to-binding "Empty list")
      (if (not(= (length ls) 2))
          (error 'parse "Binding invalido")
          (binding (car ls) (parse (second ls))))
)
)

(define (mapBindings ls acc)
    (if (empty? ls)
       empty
       (let ([iD (car (car ls))])
         (if (member iD acc)
           (error 'parse "No puede declarar 2 variables de ligado con el mismo identificador")
           (cons (list-to-binding (car ls)) (mapBindings (cdr ls) (cons iD acc)))))))

(define (no-rep ls acc)
  (if (empty? ls)
      empty
      (if (member (car ls) acc)
      (error 'parse (string-append "Parámetro '" (~a (car ls))
                                   " definido dos veces."))
      (cons (car ls) (no-rep (cdr ls) (cons (car ls) acc))))))
