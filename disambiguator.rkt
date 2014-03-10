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
    [(ambiguous _ ids) (disambiguate-ids links cenv t ids)]
    [_ (ast-transform (curry disambiguate-ast links cenv) t)]))

(define (disambiguate-ids links cenv t ids)
  (define (disambiguate-ids-helper _ids)
    (cond
      [(equal? 1 (length _ids)) (disambiguate-single-id links cenv t (first _ids))]
      [else (disambiguate-result links _ids (disambiguate-ids-helper (reverse (rest (reverse _ids)))))]))

  (define disambiguated-ids (disambiguate-ids-helper ids))
  (cond
    [(list? disambiguated-ids) (c-errorf "Could not disambiguate ids: ~a" ids)]
    [else (printf "Disambiguated: ~a to ~a~n" ids disambiguated-ids) disambiguated-ids]))
  
(define (disambiguate-single-id links cenv t id)
  (printf "RIGHT HERE, this env is for java.lang.Array's env but t is a node in the PrintStream AST~n")
  (envs-print (ast-envt t))
  (define local-var (assoc id (envs-vars (ast-envt t))))
  (define field-var (assoc id (envs-vars cenv)))
  (define typelink (assoc (list id) links))
  (cond
    [(list? local-var) (varuse id)]
    [(list? field-var) (varuse id)] ;?
    [(list? typelink) (rtype (first (second typelink)))]
    [else (list id)]))

(define (disambiguate-result links ids result)
  (cond
    [(and (list? result) (list? (assoc result links))) (fieldaccess (rtype (first (second (assoc result links)))) (last ids))]
    [(or (varuse? result) (rtype? result)) (fieldaccess result (last ids))]
    [else ids]))









