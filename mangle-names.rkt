#lang racket
(require "environments.rkt")
(require "types.rkt")
(require "errorf.rkt")
(require "generation-structures.rkt")
(provide mangle-names)


;;mangling names
(define (mangle-names thing)
  (cond [(funt? thing) (apply string-append (cons (funt-id thing) (map get-mangled-type-name (funt-argt thing))))]
        [(list? thing) (string-join thing "_")]
	[(codemeth? thing) (string-join (mangle-names (codemeth-origin thing)) "_" (mangle-names (codemeth-id thing)))]
	[(codevar? thing) (string-join (mangle-names (codevar-tag thing)) "_" (codevar-id thing))]
	[else (c-errorf "Unknown thing ~a" thing)]))

(define (get-mangled-type-name ast)
  (match ast
    [(ptype name) (string-append "_" (symbol->string name))]
    [(rtype name) (string-append "_r_" (apply string-append name))]
    [(atype t) (string-append "_a_" (get-mangled-type-name t))]))


;;class name
;;scope id 
;;variable name


;;code-meth or code-var
