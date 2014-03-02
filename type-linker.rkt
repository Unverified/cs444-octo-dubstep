#lang racket

(require "ast-tree.rkt")
(require "enviroments.rkt")

(provide gen-typelink-lists)
(provide print-all-links)
(provide (struct-out link))
(struct link (full env) )

;======================================================================================
;==== Helper Functions
;======================================================================================

;removes the last element of a list
(define (remove-last l)
  (reverse (rest (reverse l))))

; is "a" a prefix of "b"
(define (is-prefix a b) 
  (cond
    [(empty? b) #f]
    [(equal? a b) #t]
    [else (is-prefix a (remove-last b))]))

;======================================================================================
;==== Linker Generation
;======================================================================================

(define (gen-typelink-lists asts root)
  (map (lambda (ast r) (printf "############ LINKING NAMES IN FILE: ~a ############~n" (first r))
         (define enclosing-class-links (list (list (list (get-class-name ast)) (find-fully-qualified-link (c-unit-name ast) root))))
	 (define enclosing-package-links (find-default-package-links ast root))
         (define single-import-links (check-for-clashes (link-single-imports (get-package-name ast) (filter cimport? (cunit-imports ast)) root) (list (get-class-name ast))))
         (define on-demand-import-links (reverse (check-for-ondemand-clashes (link-on-demand-imports (get-package-name ast) (filter pimport? (cunit-imports ast)) root) empty)))

         ;(printf "======== enclosing-class-links ========")
         ;(print-links enclosing-class-links)

         ;(printf "~n======== enclosing-package-links ========")
         ;(print-links enclosing-package-links)

         ;(printf "~n======== single-import-links ========")
         ;(print-links single-import-links)

         ;(printf "~n======== on-demand-import-links ========")
         ;(print-links on-demand-import-links)
 
         (define links-fully-qualified (append enclosing-class-links single-import-links enclosing-package-links on-demand-import-links))
         (define links-single-type (map (lambda(r) (list (first r) (const (apply link r)))) root))

         (define class-type-links (filter-not empty? (gen-typelink-list links-fully-qualified links-single-type ast)))

         ;(printf "~n======== class-type-links ========~n~a~n"class-type-links)

	 (remove-duplicates (append class-type-links (map (lambda(x) (list (first x) ((second x))) ) links-single-type))))
         ;(append class-type-links single-import-links on-demand-import-links)) 
       asts root))

(define (gen-typelink-list links-fully-qualified links-single-type ast)
  (define (resolve-type name assoc-list)
    (match (assoc name assoc-list)
      [`(,key ,value) (value)]
      [_ (printf "Could not resolve type ~a~n" name) (print-links assoc-list) (error "")]))

  (define (typelink-helper name)
    (cond
      [(empty? name) empty]
      [(equal? 1 (length name)) (list name (resolve-type name links-fully-qualified))]
      [else (list name (resolve-type name links-single-type))]))

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

(define (find-default-package-links ast root)
  (define default-package (get-package-name ast))
  (define links (find-package-links default-package default-package root))
  (map (lambda(l) (list (first l) (const ((second l))) )) links))

(define (find-fully-qualified-link name root)
  (define r (findf (lambda(x) (equal? name (first x))) root))
  (cond
    [(list? r) (const (apply link r))]
    [else #f]))

(define (find-package-links default-package package root)
  (define (find-package-links-helper r package)
    (define r-package (remove-last (first r)))
    (cond
      [(equal? package r-package) (list (list (list (last (first r))) (const (apply link r))))]
      [(and (not (is-in-default-package (first r) default-package)) (is-prefix (first r) package)) (list (list (list (last (first r))) (const (error "Fully qualified type is clashing with package." (first r) package))))]
      [else empty]))
  (append-map (lambda(r) (find-package-links-helper r package)) root))

(define (link-on-demand-imports default-package imports root)
  (define (get-plinks package root)
    (define links (find-package-links default-package package root))
    (cond
      [(and (empty? links) (not (is-package-prefix-decl package root))) (error "Could not find a package declaration for an import on demand.")]
      [else links]))

  (append-map (lambda(x) (get-plinks (pimport-path x) root)) imports))

(define (link-single-imports default-package imports root)
  (define (get-clink name root)
    (define link (find-fully-qualified-link name root))
    (cond
      [(false? link) (error "Could not find a link for a single import.")]
      [(check-for-package-prefixs default-package (remove-last name) root) (error "Fully qualified type is clashing with package import prefix." name)]
      [else link]))
  
  (map (lambda(x) (list (list (last (cimport-path x))) (get-clink (cimport-path x) root))) imports))

(define (is-in-default-package check default-package)
  (define check-package (remove-last check))
  (equal? check-package default-package))

(define (is-package-prefix-decl package root)
  (list? (findf (lambda(x) (is-prefix package (first x))) root)))

(define (check-for-package-prefixs default-package package root)
  (list? (findf (lambda(x) (and (not (is-in-default-package (first x) default-package)) (is-prefix (first x) package))) root)))

;======================================================================================
;==== Error Checking
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
    [(list? (memf (lambda(x) (equal? x (get-package-ci (first links)))) seen-so-far)) (cons (list (first (first links)) (const (error "On demand clashing"))) 
                                                                                            (check-for-ondemand-clashes (rest links) (cons (get-package-ci (first links)) seen-so-far)))]
    [else (cons (first links) (check-for-ondemand-clashes (rest links) (cons (get-package-ci (first links)) seen-so-far)))]))

;======================================================================================
;==== Print Functions
;======================================================================================

(define (print-all-links all-links root)
  (for-each (lambda(x r) (printf "~n======= LINKS FOR FILE: ~a =======~n" (first r)) (print-links x)) all-links root) )

(define (print-links links)
  (for-each (lambda(x) (print-link x)) links))

(define (print-link l)
  (match l
    [`(,name ,(link x y)) (printf "~nTYPE: ~a LINKS TO:~n~a~n" name x)]
    [`(,name ,x) (printf "~nTYPE: ~a LINKS TO:~n~a~n" name x)]
    [`() (printf "EMPTY PAIR???~n")]))

;======================================================================================
;==== ERROR
;======================================================================================

(define (error message . args)
  (printf (string-append message " ~a") args)
  (exit 42))
    




