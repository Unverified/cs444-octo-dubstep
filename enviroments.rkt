#lang racket

(require "ast-tree.rkt")

(define (c-unit-name ast)
  (match ast
    [(or (c-unit package _ (class _ _ id _ _ _)) 
         (c-unit package _ (interface _ _ id _ _))) (append package (list id))]
    [_ (error "c-unit->env requires a ")]))


(define (gen-root-env asts)
  (map (lambda (x) (list (c-unit-name x) x)) asts))