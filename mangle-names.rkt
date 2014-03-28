#lang racket
(require "environments.rkt")
(require "types.rkt")
(require "errorf.rkt")
(provide mangle-names)


;;mangling names


(define (mangle-names funt)
  (string-append (funt-id funt) (foldr string-append "" (map get-mangled-type-name (funt-argt funt)))))

(define (get-mangled-type-name ast)
  (match ast
    [(ptype name) (string-append "_" (symbol->string name))]
    [(rtype name) (string-append "_r_" (foldr string-append "" name))]
    [(atype t) (string-append "_a_" (get-mangled-type-name t))]))
