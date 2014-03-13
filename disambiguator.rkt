#lang racket

(require "errorf.rkt")
(require "ast-tree.rkt")
(require "class-info.rkt")
(require "type-linker.rkt")
(require "environments.rkt")

(provide disambiguate)

; I AM THE DISAMBIGUATOR

; HEY LISTEN! I know what you're thinking, "Hey I could totally move this in the type-linker and be a hero!". NO! Dont fucking do that. This guy requires the heirarchy to be build so it can identifiy field variables from class in extends, if you move it to type-linker it wont have those and thus if wont know if a.foo() means class "a" or field variable "a".

(define (disambiguate cinfo rootnames)
  (map (lambda(x) (printf "~n====== DISAMBIGUATING NAMES FOR AST, class/interface: ~a ======~n~n" (first x))
                       (set-cinfo-ast x (disambiguate-ast rootnames (info-links (second x)) (info-env (second x)) (info-ast (second x))))) cinfo))

(define (disambiguate-ast rootnames import-links cenv t)
  (match t
    [(ambiguous lenv ids) (disambiguate-ids rootnames import-links cenv lenv ids)]
    [_ (ast-transform (curry disambiguate-ast rootnames import-links cenv) t)]))

(define (disambiguate-ids rootnames import-links cenv lenv ids)
  (define (disambiguate-ids-helper _ids)
    (cond
      [(equal? 1 (length _ids)) (disambiguate-single-id import-links cenv lenv (first _ids))]
      [else (disambiguate-result rootnames lenv _ids (disambiguate-ids-helper (reverse (rest (reverse _ids)))))]))

  (define disambiguated-ids (disambiguate-ids-helper ids))
  
  (cond
    [(list? disambiguated-ids) (c-errorf "Could not disambiguate ids: ~a" ids)]
    [else (printf "Disambiguated: ~a to ~a~n" ids disambiguated-ids) disambiguated-ids]))
  
(define (disambiguate-single-id import-links cenv lenv id)
  (define local-var (assoc id (envs-vars lenv)))
  (define field-var (assoc id (envs-vars cenv)))
  (define typelink (assoc (list id) import-links))
  (cond
    [(list? local-var) (varuse lenv id)]
    [(list? field-var) (varuse lenv id)] ;?
    [(list? typelink) (rtype lenv ((second typelink)))]
    [else (list id)]))

(define (disambiguate-result rootnames lenv ids result)
  (cond
    [(fieldaccess? result) (fieldaccess lenv result (last ids))]
    [(or (varuse? result) (rtype? result)) (fieldaccess lenv result (last ids))]
    [(and (list? result) (list? (member ids rootnames))) (rtype lenv (first (member ids rootnames)))]
    [else ids]))









