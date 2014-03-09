#lang racket

(require "environments.rkt")

(provide (struct-out info))
(provide print-info)

(struct info (ast env links))

(define (print-info class-info)
  (for-each (lambda(cinfo) (printf "========== CINFO FOR ~a ==========~n" (first cinfo))
                           (envs-print (info-env (second cinfo))) ;all i want to print fo now
) class-info))
