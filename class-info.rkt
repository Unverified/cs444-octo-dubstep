#lang racket

(require "environments.rkt")
(provide (struct-out info))

(provide find-info)
(provide info-path)
(provide print-info)
(provide superclass?)

(provide set-cinfo-ast)
(provide set-cinfo-env)
(provide set-cinfo-links)
(provide set-cinfo-supers)
(provide set-cinfo-impls)


(struct info (name ast env links supers impls))

(define (find-info name infolst)
  (findf (compose (curry equal? name) info-name) infolst))

(define (info-path i)
  (append (list (info-name i)) (info-supers i)))

(define (set-cinfo-ast cinfo ast)
  (info (info-name   cinfo) 
        ast
        (info-env    cinfo)
        (info-links  cinfo)
        (info-supers cinfo)
        (info-impls  cinfo)))

(define (set-cinfo-env cinfo env)
  (info (info-name   cinfo)
        (info-ast    cinfo)
        env
        (info-links  cinfo)
        (info-supers cinfo)
        (info-impls  cinfo)))

(define (set-cinfo-links cinfo links)
  (info (info-name   cinfo)
        (info-ast    cinfo) 
        (info-env    cinfo)
        links
        (info-supers cinfo)
        (info-impls  cinfo)))

(define (set-cinfo-supers cinfo supers)
  (info (info-name   cinfo) 
        (info-ast    cinfo)
        (info-env    cinfo)
        (info-links  cinfo)
        supers
        (info-impls  cinfo)))

(define (set-cinfo-impls cinfo impls)
  (info (info-name   cinfo) 
        (info-ast    cinfo)
        (info-env    cinfo)
        (info-links  cinfo)
        (info-supers cinfo)
        impls))

; checks if B is a superclass of A
(define (superclass? all-cinfo A B)
  (printf "superclass? A: ~a, B: ~a~n" A B)
  (define A-supers (cons A (info-supers (find-info A all-cinfo))))
  (printf "JHVIUFG ~a~n" A-supers)
  (list? (member B A-supers)))

(define (print-info cinfo)
  (printf "========== CINFO FOR ~a ==========~n" (string-join (info-name cinfo) "."))
  (envs-print (info-env cinfo))) ;all i want to print fo now 
