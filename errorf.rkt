#lang racket

(provide c-errorf)

;==========================================================================================
;===== Compiler Error
;==========================================================================================

(define (c-errorf message . x)
  (printf "COMPILER ERROR: ~a" (format-error message x))
  (exit 42))

(define (format-error message x)
  (define ms (regexp-split #px"~a" message))
  (define xs (append x `("")))
  (for-each (lambda(m1 x1) (printf "~a~a" m1 x1)) ms xs))
