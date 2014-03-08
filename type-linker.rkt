#lang racket

(require "ast-tree.rkt")
(require "environments.rkt")

(provide gen-typelink-lists)
(provide print-all-links)
(provide print-links)
;(provide (struct-out link))
;(struct link (full env))

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

;======================================================================================
;==== Linker Generation
;======================================================================================

(define (gen-typelink-lists asts rootenvs)
  (let ([env-names (map first rootenvs)])
    (cond
      [(not (equal? (length env-names) (length (remove-duplicates env-names))))  (error "duplicate environments have been defined")]
      [else (map (lambda (ast r) (printf "############ LINKING NAMES IN FILE: ~a ############~n" (first r))
                   (define enclosing-class-links (list (pair (list (get-class-name ast)) (find-fully-qualified-link (c-unit-name ast) rootenvs))))
                   (define enclosing-package-links (find-package-links (get-package-name ast) rootenvs))
                   (define single-import-links (check-for-clashes (link-single-imports (filter cimport? (cunit-imports ast)) rootenvs) (c-unit-name ast) empty))
                   (define on-demand-import-links (reverse (check-for-ondemand-clashes (link-on-demand-imports (filter pimport? (cunit-imports ast)) rootenvs) empty)))
                   
                   ;(printf "======== enclosing-class-links ========")
                   ;(print-links enclosing-class-links)
                   
                   ;(printf "~n======== enclosing-package-links ========")
                   ;(print-links enclosing-package-links)
                   
                   ;(printf "~n======== single-import-links ========")
                   ;(print-links single-import-links)
                   
                   ;(printf "~n======== on-demand-import-links ========")
                   ;(print-links on-demand-import-links)
                   
                   (define possible-typename-links (append enclosing-class-links single-import-links enclosing-package-links on-demand-import-links))
                   (define rootlinks (check-and-get-rootlinks ast rootenvs))
                   
                   (define package-prefixes 
                     (remove-duplicates (append (get-all-prefixes (get-package-name ast)) 
                                                (append-map (lambda(ci) (get-all-prefixes (remove-last (cimport-path ci)))) (filter cimport? (cunit-imports ast)))
                                                (append-map (lambda(pi) (get-all-prefixes (pimport-path pi))) (filter pimport? (cunit-imports ast))))))
                   
                   (gen-typelink-list ast possible-typename-links rootlinks))
                 asts rootenvs)])))

(define (gen-typelink-list ast possible-typename-links rootlinks)
  (define (resolve-type typename assoc-list)
    (define typelink (assoc typename assoc-list))
    (cond
      [(and (list? typelink) (procedure? (second typelink))) ((second typelink))] ;if its a procedure than there is an error associated with using this link
      [(list? typelink) (second typelink)]
      [else (error "Could not resolve typename:" typename)]))
  
  (define (typelink-helper typename)
    (cond
      [(empty? typename) empty]
      [(equal? 1 (length typename)) (resolve-type (check-typename-prefix-not-type typename possible-typename-links) possible-typename-links)]
      [else (resolve-type (check-typename-prefix-not-type typename possible-typename-links) rootlinks)]))
  
  (define (typelink ast)
    (match ast
      [(interface _ s m id e b) (interface s m id (map (lambda(x) (typelink-helper x)) e) (typelink b))]
      
      [(class _ s m id e i b) (class s m id (typelink-helper e) (map (lambda(x) (typelink-helper x)) i) (typelink b))]
      
      [(rtype _ t) (rtype (typelink-helper t))]
                   ;(cond
                   ;  [(list? t) (cons (typelink-helper t) empty)]
                   ;  [else (typelink t)])]
      
      ;[(atype _ t) (cond
      ;               [(list? t) (cons (typelink-helper t) empty)]
      ;               [else (typelink t)])]
      
      ;[(cast _ c expr) (cond
      ;                   [(list? c) (cons (typelink-helper c) (typelink expr))]
      ;                   [else (typelink expr)])]
      
      ;[(arraycreate _ t expr) (cond
      ;                          [(list? t) (cons (typelink-helper t) (typelink expr))]
      ;                          [else (typelink expr)])]
      
      ;[(classcreate _ t args) (cons (typelink-helper t) (typelink args))]

      [_ (ast-transform typelink ast)]))
  (typelink ast))

;======================================================================================
;==== Import Linker Generation
;======================================================================================

(define (find-fully-qualified-link name rootenvs)
  (define r (findf (lambda(x) (equal? name (first x))) rootenvs))
  (cond
    [(list? r) (first r)]
    [else #f]))

(define (find-package-links package rootenvs)
  (define (find-package-links-helper r package)
    (define r-package (remove-last (first r)))
    (cond
      [(equal? package r-package) (list (pair (list (last (first r))) (first r)))]
      [else empty]))
  (append-map (lambda(r) (find-package-links-helper r package)) rootenvs))

(define (link-on-demand-imports imports rootenvs)
  (define (get-plinks package rootenvs)
    (define links (find-package-links package rootenvs))
    (cond
      [(and (empty? links) (not (is-package-prefix-decl package rootenvs))) (error "Could not find a package declaration for an import on demand.")]
      [else links]))
  (append-map (lambda(x) (get-plinks (pimport-path x) rootenvs)) imports))

(define (link-single-imports imports rootenvs)
  (define (get-clink name rootenvs)
    (define link (find-fully-qualified-link name rootenvs))
    (cond
      [(false? link) (error "Could not find a link for a single import.")]
      [else link]))
  
  (map (lambda(x) (list (list (last (cimport-path x))) (get-clink (cimport-path x) rootenvs))) imports))


;======================================================================================
;==== Error Checking
;======================================================================================

(define (check-and-get-rootlinks ast rootenvs)
  (define package-prefixes 
    (remove-duplicates (append (get-all-prefixes (get-package-name ast)) 
                               (append-map (lambda(ci) (get-all-prefixes (remove-last (cimport-path ci)))) (filter cimport? (cunit-imports ast)))
                               (append-map (lambda(pi) (get-all-prefixes (pimport-path pi))) (filter pimport? (cunit-imports ast))))))
  (map (lambda(x) (pair (check-no-prefix-resolves-to-type x package-prefixes) (first x))) rootenvs))

(define (check-no-prefix-resolves-to-type rootenv package-prefixes)
  (cond
    [(empty? (remove-last (first rootenv))) (first rootenv)]	;if rootenv is in the default package (ie single typename, no dots in it)
    [(list? (member (first rootenv) package-prefixes)) (error "check-no-prefix-resolves-to-type failed." (first rootenv))]
    [else (first rootenv)]))

(define (check-typename-prefix-not-type typename possible-typename-links)
  (define find-prefix (memf (lambda(x) (assoc x possible-typename-links)) (get-all-prefixes (remove-last typename))))
  (cond [(list? find-prefix) (error "check-typename-prefix-not-type failed." typename (first find-prefix))]
        [else typename]))

(define (is-package-prefix-decl package rootenvs)
  (list? (findf (lambda(x) (prefix? package (remove-last (first x)))) rootenvs)))

(define (check-for-clashes links enclosing-type seen-so-far)
  (cond
    [(empty? links) empty]
    [(check-for-class-name-clash enclosing-type (first links)) (error "Single import clashing with class name.")]
    [(list? (memf (lambda(x) (equal? x (first (first links)))) seen-so-far)) (error "Single import clashing another import.")]
    [else (cons (first links) (check-for-clashes (rest links) enclosing-type (cons (first (first links)) seen-so-far)))]))

(define (check-for-class-name-clash enclosing-type import-typelink)
  (and (not (equal? enclosing-type (pair-value import-typelink))) (equal? (list (last enclosing-type)) (pair-key import-typelink)) ))

(define (check-for-ondemand-clashes links seen-so-far)
  (define (get-package-ci link) (last (first link)))
  (cond
    [(empty? links) empty]
    [(list? (memf (lambda(x) (equal? x (get-package-ci (first links)))) seen-so-far)) (cons (list (first (first links)) (lambda()(error "On demand clashing" (get-package-ci (first links))))) 
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
    [`(,name ,x) (printf "~nTYPE: ~a LINKS TO:~n~a~n" name x)]
    [`() (printf "EMPTY PAIR???~n")]))

;======================================================================================
;==== ERROR
;======================================================================================

(define (error message . args)
  (printf (string-append message " ~a") args)
  (exit 42))





