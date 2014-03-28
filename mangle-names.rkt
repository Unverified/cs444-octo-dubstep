#lang racket
(require "environments.rkt")
(require "types.rkt")
(require "errorf.rkt")
(provide mangle-names)


;;mangling names


(define (mangle-names thing)
	(match thing
		[(funt id params) (string-append id (foldr string-append "" (map get-mangled-type-name params))))]
		[(list cname scope-id var-name) (string-append cname "_" scope-id "_" var-name "_" "var")]))

(define (get-mangled-type-name ast)
  (match ast
    [(ptype name) (string-append "_" (symbol->string name))]
    [(rtype name) (string-append "_r_" (foldr string-append "" name))]
    [(atype t) (string-append "_a_" (get-mangled-type-name t))]))


;;class name
;;scope id 
;;variable name



