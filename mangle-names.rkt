#lang racket
(require "environments.rkt")
(require "types.rkt")
(require "errorf.rkt")
(require "generation-structures.rkt")

(provide constr-label)
(provide mangle-names)

(define (constr-label class args)
  (cond [(not (and (list? class) (andmap string? class)))
         (error 'constr-label "given invalid class name ~e" class)]
        [(not (and (list? args) (andmap (lambda (x) (or (atype? x) (rtype? x) (ptype? x))) args)))
         (error 'constr-label "given invalid arg list ~e" args)]
        [else (mangle-names (codemeth (funt "" args) #f #f class 0 empty))]))

;;mangling names
(define (mangle-names thing)
  (cond [(funt? thing) (apply string-append (cons (funt-id thing) (map get-mangled-type-name (funt-argt thing))))]
        [(list? thing) (string-join thing "_")]
	[(codemeth? thing) (string-append (mangle-names (codemeth-origin thing)) "_" (mangle-names (codemeth-id thing)) "_method")]
	[(codevar? thing) (string-append (mangle-names (codevar-tag thing)) "_" (codevar-id thing) "_var")]
        [(codeenv? thing) (string-append (mangle-names (codeenv-name thing)) "_" (number->string (codeenv-guid thing)) "_class")]
	[else (c-errorf "NAME MANGLER ERROR: Unrecognized structure/object: ~a" thing)]))

(define (get-mangled-type-name ast)
  (match ast
    [(ptype name) (string-append "_" (symbol->string name))]
    [(rtype name) (string-append "_r_" (apply string-append name))]
    [(atype t) (string-append "_a_" (get-mangled-type-name t))]))


;;class name
;;scope id 
;;variable name


;;code-meth or code-var
