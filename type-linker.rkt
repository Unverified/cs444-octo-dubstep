#lang racket

(define (gen-root-env asts)
  (map (lambda (x) (list (c-unit-name x) x)) asts))
