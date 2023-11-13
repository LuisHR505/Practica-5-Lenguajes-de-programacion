#lang plai

(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./interp.rkt"))

(define (prueba e)
  (interp (parse e) (mt-env)))

(test (parse 'x) (id 'x)) ;; si pasa
(test (parse '2) (num 2)) ;; si pasa
(test (parse '#t) (bool #t));;si pasa
(test (parse '"Hola") (strinG "Hola")) ;;si pasa
(test (parse '{sub1 2}) (op sub1 (list (num 2)))) ;;pasa individual
(test/exn (parse '{sub1 2 3}) "parse: La operación sub1 debe ser ejecutada con 1 argumentos.") ;;pasa individual
(test/exn (parse '{min}) "parse: La operación min debe ser ejecutada con mas de 0 argumentos.");;pasa individual
(test (parse '{rec {{foo {fun {x} x}} {a 10} {b {+ 1 a}}} {foo b}}) ;;pasa individual
      (rec
          (list (binding 'foo (fun '(x) (id 'x))) (binding 'a (num 10)) (binding 'b (op + (list (num 1) (id 'a)))))
        (app (id 'foo) (list (id 'b))))) ;;pasa individual

(test/exn (prueba 'foo) "interp: Variable libre foo") ;;no pasa
(test (prueba '1234) (num-v 1234)) ;; si pasa
(test (prueba '#t) (bool-v #t)) ;;si pasa
(test (prueba '#f) (bool-v #f)) ;;si pasa
(test (prueba '"Hi") (string-v "Hi")) ;;si pasa
(test (prueba '{+ 1 2 3}) (num-v 6)) ;;si pasa
(test (prueba '{- 1 2 3}) (num-v -4)) ;;si pasa
(test (prueba '{* 1 2 -3}) (num-v -6)) ;;si pasa
(test (prueba '{* 1 2 -3 0}) (num-v 0)) ;;si pasa
(test (prueba '{/ 1 2}) (num-v (/ 1 2))) ;;si pasa
(test (prueba '{min 20 90 -1 0}) (num-v -1)) ;;si pasa
(test (prueba '{max 20 90 -1 0}) (num-v 90)) ;;si pasa
(test (prueba '{sqrt 20}) (num-v 4.47213595499958)) ;;si pasa
(test (prueba '{modulo 3 2}) (num-v 1)) ;;si pasa
(test (prueba '{sub1 3}) (num-v 2)) ;;si pasa
(test (prueba '{sub1 (add1 (expt 3 3))}) (num-v 27)) ;;si pasa
(test (prueba '{str-length "Hello"}) (num-v 5)) ;;no pasa
(test (prueba '{> 10 9}) (bool-v #t)) ;;si pasa
(test (prueba '{> 1 2}) (bool-v #f)) ;;si pasa
(test (prueba '{> 10 9 8 7 6 5 4 3 2 1}) (bool-v #t)) ;;si pasa
(test (prueba '{> 1 2 3 4 5 6 7 8}) (bool-v #f)) ;;si pasa
(test (prueba '{= 1 2 3 4 5 6 7 8}) (bool-v #f)) ;si pasa
(test (prueba '{= 10 10}) (bool-v #t)) ;;si pasa
(test (prueba '{zero? 10}) (bool-v #f)) ;;si pasa
(test (prueba '{zero? 0}) (bool-v #t)) ;;si pasa
(test (prueba '{num? 10}) (bool-v #t)) ;;no pasa
(test (prueba '{bool? 10}) (bool-v #f));;si pasa
(test (prueba '{bool? {and {zero? {add1 1}}
                           {num? "Hello"}}}) (bool-v #t)) ;;no pasa
(test (prueba '{or {zero? {+ -1 1}}
                   {num? {sub1 0}}
                   {bool? {str? "Hi"}}}) (bool-v #t)) ;; no pasa
(test/exn (prueba 'a) "interp: Variable libre a") ;;no pasa
(test (prueba '{rec {[x 2] [y 3]} {+ x 3 y}}) (num-v 8)) ;; no pasa
(test (prueba '{rec {{x 5} {y 1}} {+ x y}}) (num-v 6)) ;; no pasa
(test (prueba '{rec {{x 5} {y {+ x 1}}} {+ x y}}) (num-v 11)) ;; no pasa
(test (prueba'{rec {{a 2} {b {+ a a}}} b}) (num-v 4)) ;;no pasa
(test/exn (prueba'{{fun {y x} x} 1 y}) "interp: Variable libre y") ;;no pasa
(test/exn (prueba '{rec {{x y} {y 1}} x}) "interp: Variable libre y") ;;no pasa
(test (prueba'{rec ([x 2] [y 1] [z 3]) (/ x y z)}) (num-v 2/3)) ;; no pasa

(test (prueba '{rec ([x 1]
                      [y 1])
                     {rec ([x 5]
                            [y x])
                           (+ x y)}}) (num-v 10)) ;;no pasa
(test (prueba '{rec ([x 1]
                      [y 1])
                     {rec ([x 5]
                             [y x])
                            (+ x y)}}) (num-v 10));; no pasa

(test (prueba '{rec {{a 10}
                        {foo {fun {x} {if {zero? x}
                                          a
                                          {add1 {foo {sub1 x}}}}}}
                        {a 20}
                        {b {foo 2}}}
                       {+ a {foo 10}}}) (num-v 40));;no pasa

(test/exn (prueba '{rec {{foo {fun {x} {if {zero? x}
                                       a
                                       {add1 {foo {sub1 x}}}}}}
                     {a 10}}
                 {foo 10}}) "interp: Variable libre a") ;;no pasa
