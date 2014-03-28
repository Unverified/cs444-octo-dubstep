#lang racket
(require "environments.rkt")
(require "types.rkt")
(require "errorf.rkt")
(provide mangle-names)


;;mangling names
(define (mangle-names thing)
  (cond [(funt? thing) (apply string-append (cons (funt-id thing) (map get-mangled-type-name (funt-argt thing))))]
        [(list? thing) (string-join thing "_")]))

(define (get-mangled-type-name ast)
  (match ast
    [(ptype name) (string-append "_" (symbol->string name))]
    [(rtype name) (string-append "_r_" (apply string-append name))]
    [(atype t) (string-append "_a_" (get-mangled-type-name t))]))


;;class name
;;scope id 
;;variable name



