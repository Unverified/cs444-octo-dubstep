#lang racket
(require "environments.rkt")
(require "types.rkt")
(require "errorf.rkt")
(provide mangle-names)


;;mangling names

(define (mangle-names funt)
  (apply string-append (cons (funt-id funt) (map get-mangled-type-name (funt-argt funt)))))

(define (get-mangled-type-name ast)
  (match ast
    [(ptype name) (string-append "_" (symbol->string name))]
    [(rtype name) (string-append "_r_" (apply string-append name))]
    [(atype t) (string-append "_a_" (get-mangled-type-name t))]))
