#lang racket

(provide c-errorf)

;==========================================================================================
;===== Compiler Error
;==========================================================================================

(define (c-errorf message . x)
  (printf "COMPILER ERROR: ")
  (cond
    [(equal? `() x) (printf "~a" message)]
    [else (for-each (lambda(m1 m2) (printf "~a~a" m1 m2)) (regexp-split #px"~a" message) (append x `("")))])
  (printf "~n")
  (exit 42))

