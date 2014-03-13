#lang racket

(require "environments.rkt")

(provide (struct-out info))

(provide set-cinfo-ast)
(provide set-cinfo-env)
(provide set-cinfo-links)
(provide print-info)

(struct info (ast env links))

(define (set-cinfo-ast cinfo ast)
  (list (first cinfo) (info ast (info-env (second cinfo)) (info-links (second cinfo)))))

(define (set-cinfo-env cinfo env)
  (list (first cinfo) (info (info-ast (second cinfo)) env (info-links (second cinfo)))))

(define (set-cinfo-links cinfo links)
  (list (first cinfo) (info (info-ast (second cinfo)) (info-env (second cinfo)) links)))

(define (print-info class-info)
  (for-each (lambda(cinfo) (printf "========== CINFO FOR ~a ==========~n" (first cinfo))
                           (envs-print (info-env (second cinfo))) ;all i want to print fo now
) class-info))
