#lang racket

(require "ast-tree.rkt")

(provide gen-root-env)
(provide print-envs)

(define (c-unit-name ast)
  (match ast
    [(or (c-unit package _ (class _ _ id _ _ _)) 
         (c-unit package _ (interface _ _ id _ _))) (append package (list id))]
    [_ (error "c-unit->env requires a ")]))


(define (gen-root-env asts)
  (map (lambda (x) (list (c-unit-name x) x)) asts))

;==============================================================================================
;==== Print Functions
;==============================================================================================

(define (print-env env)
  (printf "~a~n" (first env)))

(define (print-envs envs)
  (for-each (lambda (env) (print-env env)) envs))
