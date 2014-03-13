#lang racket

(require "errorf.rkt")
(require "ast-tree.rkt")
(require "class-info.rkt")
(require "type-linker.rkt")
(require "environments.rkt")

(provide disambiguate)

; I AM THE DISAMBIGUATOR

(define (disambiguate cinfo)
  (for-each (lambda(x) (printf "~n====== DISAMBIGUATING NAMES FOR AST, class/interface: ~a ======~n~n" (first x))
              (disambiguate-ast (info-links (second x)) (info-env (second x)) (info-ast (second x)))) cinfo))

(define (disambiguate-ast links cenv t)
  (match t
    [(ambiguous lenv ids) (disambiguate-ids links cenv lenv ids)]
    [_ (ast-transform (curry disambiguate-ast links cenv) t)]))

(define (disambiguate-ids links cenv lenv ids)
  (define (disambiguate-ids-helper _ids)
    (cond
      [(equal? 1 (length _ids)) (disambiguate-single-id links cenv lenv (first _ids))]
      [else (disambiguate-result links lenv _ids (disambiguate-ids-helper (reverse (rest (reverse _ids)))))]))
  
  (define disambiguated-ids (disambiguate-ids-helper ids))
  
  (cond
    [(list? disambiguated-ids) (c-errorf "Could not disambiguate ids: ~a" ids)]
    [else (printf "Disambiguated: ~a to ~a~n" ids disambiguated-ids) disambiguated-ids]))

(define (disambiguate-single-id links cenv lenv id)
  (print-links links)
  (define local-var (assoc id (envs-vars lenv)))
  (define field-var (assoc id (envs-vars cenv)))
  (define typelink (assoc (list id) links))
  (cond
    [(list? local-var) (varuse lenv id)]
    [(list? field-var) (varuse lenv id)] ;?
    [(list? typelink) (rtype lenv ((second typelink)))]
    [else (list id)]))

(define (disambiguate-result links lenv ids result)
  (cond
    [(fieldaccess? result) (fieldaccess lenv result (last ids))]
    [(or (varuse? result) (rtype? result)) (fieldaccess lenv result (last ids))]
    [(and (list? result) (list? (assoc ids links))) (rtype lenv ((second (assoc ids links))))]	;if the result wasnt dis-ambiguated and theres a link to it
    [else ids]))









