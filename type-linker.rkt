#lang racket

(require "ast-tree.rkt")
(require "enviroments.rkt")

(provide gen-typelink-lists)
(provide print-all-links)

;======================================================================================
;==== Linker Generation
;======================================================================================

(define (gen-typelink-lists asts root)
  (map (lambda (ast r) (printf "LINKING NAMES IN FILE: ~a~n" (first r)) 
                                        (gen-typelink-list (append (list (list (list (get-class-name ast)) (find-fully-qualified-link (c-unit-name ast) root)))
                                                           (check-for-clashes (link-single-imports (filter cimport? (cunit-imports ast)) root) (list (get-class-name ast)))
                                              (find-package-links (get-package-name ast) root)
                                              (reverse (check-for-ondemand-clashes (link-on-demand-imports (filter pimport? (cunit-imports ast)) root) empty))
                                              (map (lambda(r) (list (first r) (const (second r)))) root)) root ast)) 
                    asts root))

(define (gen-typelink-list linked-imports root ast)
  (define (resolve-type name)
    (match (assoc name linked-imports)
      [`(,key ,value) (value)]
      [_ (printf "Could not resolve type ~a~n" name) (print-links linked-imports) (error "")]))

  (define (typelink-helper name)
    (cond
      [(empty? name) empty]
      [else (list name (resolve-type name))]))

  (define (typelink ast)
    (match ast

      [(interface _ _ id e b) (append (list (typelink-helper e)) (typelink b))]

      [(class _ _ id e i b) (append  (list (typelink-helper e))
                                     (map (lambda(x) (typelink-helper x)) i)
                                     (typelink b))]

      [(rtype t) (cond
                   [(list? t) (cons (typelink-helper t) empty)]
                   [else (typelink t)])]

      [(atype t) (cond
                   [(list? t) (cons (typelink-helper t) empty)]
                   [else (typelink t)])]

      [_ (ast-recurse ast typelink)]))

  (typelink ast))

;======================================================================================
;==== Import Linker Generation
;======================================================================================

(define (find-fully-qualified-link name root)
  (define r (findf (lambda(x) (equal? name (first x))) root))
  (cond
    [(list? r) (const (second r))]
    [else #f]))

(define (find-package-links package root)
  (define (find-package-links-helper r package)
    (define r-package (reverse (rest (reverse (first r)))))
    (cond
      [(equal? package r-package) (list (list (list (last (first r))) (const (second r))))]
      [else empty]))
  (append-map (lambda(r) (find-package-links-helper r package)) root))

(define (link-on-demand-imports imports root)
  (define (get-plinks package root)
    (define links (find-package-links package root))
    (cond
      [(empty? links) (error "Could not find a package declaration for an import on demand.")]
      [else links]))

  (append-map (lambda(x) (get-plinks (pimport-path x) root)) imports))

(define (link-single-imports imports root)
  (define (get-clink name root)
    (define link (find-fully-qualified-link name root))
    (cond
      [(false? link) (error "Could not find a link for a single import.")]
      [else link]))
  
  (map (lambda(x) (list (list (last (cimport-path x))) (get-clink (cimport-path x) root))) imports))

;======================================================================================
;==== Import clash checking
;======================================================================================

(define (check-for-clashes links seen-so-far)
  (cond
    [(empty? links) empty]
    [(list? (memf (lambda(x) (equal? x (first (first links)))) seen-so-far)) (error "Single import clashing.")]
    [else (cons (first links) (check-for-clashes (rest links) (cons (first (first links)) seen-so-far)))]))

(define (check-for-ondemand-clashes links seen-so-far)
  (define (get-package-ci link) (last (first link)))
  (cond
    [(empty? links) empty]
    [(list? (memf (lambda(x) (equal? x (get-package-ci (first links)))) seen-so-far)) (cons (list (first (first links)) (lambda () (error "On demand clashing"))) 
                                                                                            (check-for-ondemand-clashes (rest links) (cons (get-package-ci (first links)) seen-so-far)))]
    [else (cons (first links) (check-for-ondemand-clashes (rest links) (cons (get-package-ci (first links)) seen-so-far)))]))

;======================================================================================
;==== Print Functions
;======================================================================================

(define (print-all-links all-links)
  (for-each (lambda(x) (printf "~n--- LINKS ---~n") (print-links x)) all-links) )

(define (print-links links)
  (for-each (lambda(x) (print-link x)) links))

(define (print-link l)
  (match l
    [`(,name ,env) (printf "~nTYPE: ~a LINKS TO:~n~a~n" name env)]
    [`() (printf "")]))

;======================================================================================
;==== ERROR
;======================================================================================

(define (error message)
  (printf "ERROR: ~a~n" message)
  (exit 42))
    




