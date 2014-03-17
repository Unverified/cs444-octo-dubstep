#lang racket

(require "errorf.rkt")
(require "ast-tree.rkt")
(require "class-info.rkt")
(require "environments.rkt")

(provide gen-typelink-lists)
(provide print-all-links)
(provide print-links)
(provide get-fullname)

;======================================================================================
;==== Helper Functions
;======================================================================================

(define (pair-key p) (first p))
(define (pair-value p) (second p)) ;Elements are stored as constant functions, ambiguity creates errors
(define (pair key value) (list key value))

;removes the last element of a list
(define (remove-last l)
  (reverse (rest (reverse l))))

(define (prefix? pre lst)
  (list? (remove-prefix pre lst)))

(define (remove-prefix pre lst)
  (cond [(empty? pre) lst]
        [(empty? lst) #f]
        [(equal? (first pre) (first lst)) (remove-prefix (rest pre) (rest lst))]
        [else #f]))

(define (get-all-prefixes lst)
  (foldr (lambda (ele lst) (cons (list ele) (map (curry cons ele) lst))) empty lst))

(define filter-classes (compose1 (curry filter (compose1 (curry equal? 1) length)) 
                                 (curry filter list?)))

(define cunit-cimports (compose1 (curry filter cimport?) cunit-imports))
(define cunit-pimports (compose1 (curry filter pimport?) cunit-imports))

;======================================================================================
;==== Linker Generation
;======================================================================================
(define (create-link-pair x) (pair (first x) (const (second x)))) 

(define (gen-typelink-list root ast)
  (printf "############ LINKING NAMES IN CLASS: ~a ############~n" (c-unit-name ast))
  (define root-nodef (filter-not (compose1 (curry equal? 1) length) root))
  (let ([root-lnk    (map (lambda (x) (pair x (const x))) root)]
        [class-lnk   (list (pair (list (get-class-name ast)) (const (c-unit-name ast))))]
        [cimport-lnk (map create-link-pair (check-for-clashes (link-single-imports (cunit-cimports ast) root) (c-unit-name ast) empty))]
        [package-lnk (map create-link-pair (find-package-links (get-package-name ast) root))]
        [pimport-lnk (reverse (check-for-ondemand-clashes (link-on-demand-imports (cunit-pimports ast) root-nodef) empty))])
    (let ([all-lnk (append class-lnk cimport-lnk package-lnk pimport-lnk)])
      (info (c-unit-name ast) (typelink-ast ast all-lnk root-lnk) empty all-lnk empty empty))))

(define (gen-typelink-lists asts root)
  (cond
    [(not (equal? (length root) (length (remove-duplicates root)))) (c-errorf "duplicate compilation units have been defined")]
    [(not (empty? (clashing-cunit-packages (filter-not (compose1 (curry equal? 1) length) root)))) (c-errorf "a package name is clashing with a class ~a" (clashing-cunit-packages root))]
    [else (map (curry gen-typelink-list root) asts)]))

(define (typelink-ast ast all-links root)
  (define (typelink-helper typename)
    (cond
      [(empty? typename) empty]
      [(equal? 1 (length typename)) (get-fullname (check-typename-prefix-not-type typename all-links) all-links)]
      [else                         (get-fullname (check-typename-prefix-not-type typename all-links) root)]))
  
  (define (typelink ast)
    (match ast
      [(interface env s m id e b) (interface env s m id (map typelink-helper e) (typelink b))]
      [(class env s m id e i b) (class env s m id (typelink-helper e) (map typelink-helper i) (typelink b))]
      [(rtype t) (rtype (typelink-helper t))]
      [(this _ _) (this empty (rtype ((second (first all-links)))))]
      [_ (ast-transform typelink ast)]))
  (typelink ast))

;======================================================================================
;==== Import Linker Generation
;======================================================================================

(define (get-rootenv-link rootenvs fullname)
  (cond
    [(list? (member fullname rootenvs)) (pair (list (last fullname)) fullname)]
    [else (c-errorf "Could not find a link for a single import.")]))

(define (find-package-links package rootenvs)  
  (map (lambda (x) (pair x (append package x))) (filter-classes (map (curry remove-prefix package) rootenvs))))

(define (link-on-demand-imports imports rootenvs)
  (let* ([linklist (map (compose1 (curryr find-package-links rootenvs) pimport-path) imports)]
         [empty-import (filter-map (lambda (x y) (and (empty? x) y)) linklist (map pimport-path imports))]
         [invalid-import (filter-not (lambda (x) (ormap (curry prefix? x) rootenvs)) empty-import)])
    (cond [(empty? invalid-import) (apply append linklist)]         
          [else (c-errorf "Import on demand references package that doesnt exit~n~a~n" invalid-import)])))

(define (link-single-imports imports rootenvs)
  (map (compose1 (curry get-rootenv-link rootenvs) cimport-path) imports))

;======================================================================================
;==== Error Checking
;======================================================================================
(define occurs? (compose list? member))

(define (clashing-cunit-packages rootenvs)
  (define packages (remove-duplicates (append-map get-all-prefixes (map remove-last rootenvs))))
  (filter (curryr occurs? packages) rootenvs))

(define (check-no-prefix-resolves-to-type rootenv package-prefixes)
  (cond
    [(empty? (remove-last (first rootenv))) (first rootenv)]	;if rootenv is in the default package (ie single typename, no dots in it)
    [(list? (member (first rootenv) package-prefixes)) (c-errorf "check-no-prefix-resolves-to-type failed. ~a" (first rootenv))]
    [else (first rootenv)]))

(define (check-typename-prefix-not-type typename possible-typename-links)
  (define find-prefix (memf (lambda(x) (assoc x possible-typename-links)) (get-all-prefixes (remove-last typename))))
  (cond [(list? find-prefix) (c-errorf "check-typename-prefix-not-type failed. tn: ~a, prefix: ~a" typename (first find-prefix))]
        [else typename]))

(define (is-package-prefix-decl package rootenvs)
  (list? (findf (lambda(x) (prefix? package (remove-last (first x)))) rootenvs)))

(define (check-for-clashes links enclosing-type seen-so-far)
  (cond
    [(empty? links) empty]
    [(check-for-class-name-clash enclosing-type (first links)) (c-errorf "Single import clashing with class name.")]
    [(list? (memf (curry equal? (first (first links))) seen-so-far)) (c-errorf "Single import clashing another import.")]
    [else (cons (first links) (check-for-clashes (rest links) enclosing-type (cons (first (first links)) seen-so-far)))]))

(define (check-for-class-name-clash enclosing-type import-typelink)
  (and (not (equal? enclosing-type (pair-value import-typelink)))
       (equal? (list (last enclosing-type)) (pair-key import-typelink))))

(define (check-for-ondemand-clashes links seen-so-far)
;  (printf "----- Links ~a~n" links)
;  (printf "----- Seen ~a~n" seen-so-far)
  (define (get-package-ci link) (last (first link)))
  (cond
    [(empty? links) empty]
    [(list? (memf (curry equal? (get-package-ci (first links))) seen-so-far)) (cons (list (first (first links)) (curry c-errorf "On demand clashing ~a" (get-package-ci (first links))))
                                                                                    (check-for-ondemand-clashes (rest links) (cons (get-package-ci (first links)) seen-so-far)))]
    [else (cons (create-link-pair (first links)) (check-for-ondemand-clashes (rest links) (cons (get-package-ci (first links)) seen-so-far)))]))

;======================================================================================
;==== Getters
;======================================================================================

(define (get-fullname typename links)
  (printf "Looking for ~a in ~a~n" typename links)
  (match (assoc typename links)
    [#f (c-errorf "Could not resolve typename: ~a " typename)]
    [`(,_ ,x)  (x)]))

;======================================================================================
;==== Print Functions
;======================================================================================

(define (print-all-links all-links root)
  (for-each (lambda(x r) (printf "~n======= LINKS FOR FILE: ~a =======~n" (first r)) (print-links x)) all-links root) )

(define (print-links links)
  (for-each (lambda(x) (print-link x)) links))

(define (print-link l)
  (match l
    [`(,name ,x) (printf "~nTYPE: ~a LINKS TO:~n~a~n" name x)]
    [`() (printf "EMPTY PAIR???~n")]))
