#lang racket

(require "ast-tree.rkt")

(provide print-envs)
(provide gen-root-env)
(provide gen-class-envs)

(provide (struct-out envs))

(struct envs (vars methods types) #:transparent)

;======================================================================================
;==== Environment Generation
;======================================================================================
(define (gen-root-env asts)
  (map (lambda (x) (list (c-unit-name x) (gen-class-envs x))) asts))

(define (gen-class-envs ast)
  (define (gen-class-env-id scope ast)
    (match ast
      [(constructor scop mdecl _)       (envs empty `((,mdecl ,scope))  empty)]
      [(method scop mod type mdecl _)    (envs empty `((,mdecl ,scope)) `((,mdecl ,type)))]
      [(or (var _ _ type (varassign id _))
           (var _ _ type id))              (envs `((,id ,scope)) empty `((,id ,type)))]
      [_ env-empty]))
  
    (match ast
      [(cunit _ _ b) (gen-class-envs b)]
      [(class _ _ id _ _ b) (let ([e (gen-class-envs b)])
                              (envs (envs-vars e) (envs-methods e) (cons (list id 'class) (envs-types e))))]
      [(interface _ _ id _ b) (let ([e (gen-class-envs b)])
                              (envs (envs-vars e) (envs-methods e) (cons (list id 'interface) (envs-types e))))]
      [(block id bdy) (apply env-append (map (curry gen-class-env-id id) bdy))]))

;======================================================================================
;==== Environment Transformation
;======================================================================================
;(: env-append-1 : envs envs -> envs )
(define (env-append-1 le re)
  (envs (append (envs-vars le) (envs-vars re))
        (append (envs-methods le) (envs-methods re))
        (append (envs-types le) (envs-types re))))

;(: env-append : envs envs... -> envs )
(define (env-append le . r)
  (foldr env-append-1 le r))
  
;======================================================================================
;==== Bases
;======================================================================================
(define env-empty (envs empty empty empty))

;==============================================================================================
;==== Print Functions
;==============================================================================================

(define (print-env env)
  (printf "~a~n" (first env)))

(define (print-envs envs)
  (for-each (lambda (env) (print-env env)) envs))




(define test1 (cunit '() '() (class 'public '() "test1" '() '(("java" "io" "Serializable"))
  (block
   'g47330
   (list (constructor 'public (methoddecl "test1" '()) (block 'g47331 '()))
         (var 'public '() (ptype 'int) "x")
         (var 'public '() (ptype 'int) (varassign "y" "2"))
         (method 'public '(static) (ptype 'int) (methoddecl "test" '()) (block 'g47332 (list (return "123"))))
         )))))

(define (gen-test1)
  (gen-class-envs test1))
