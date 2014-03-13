#lang racket

(require "environments.rkt")
(provide (struct-out info))

(provide find-info)
(provide set-cinfo-ast)
(provide set-cinfo-env)
(provide set-cinfo-links)
(provide print-info)

(struct info (name ast env links supers impls))

(define (find-info name infolst)
  (findf (compose (curry equal? name) info-name) infolst))

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

(define (print-info cinfo)
  (printf "========== CINFO FOR ~a ==========~n" (string-join (info-name cinfo) "."))
  (envs-print (info-env cinfo))) ;all i want to print fo now 
