#lang racket

(require "ast-tree.rkt")

(provide gen-typelink-lists)

(struct link (ci name decl) #:transparent)

;======================================================================================
;==== Linker Generation
;======================================================================================

(define (gen-typelink-lists asts root)
  (print-all-links (map (lambda (ast) (check-for-clashes (gen-typelink-imports (cunit-imports ast) root) (list (get-class-name ast)))) asts)))

(define (gen-typelink-list ast root)
  (define (typelink-helper ci name decl)
    (cond
      [(empty? name) empty]
      [else (link ci name decl)]))

  (define (typelink-class ci ast)
    (define (typelink ast)
      (match ast

        [(interface _ _ id e b) (append (list (typelink-helper id e "decl")) (typelink-class id b))]

        [(class _ _ id e i b) (append  (list (typelink-helper id e "decl"))
                                       (map (lambda(x) (typelink-helper id x "decl")) i)
                                       (typelink-class id b))]

        [(rtype t) (cond
                     [(list? t) (cons (typelink-helper ci t "decl") empty)]
                     [else (typelink t)])]

        [(atype t) (cond
                     [(list? t) (cons (typelink-helper ci t "decl") empty)]
                     [else (typelink t)])]

        [_ (ast-recurse ast typelink)]))

    (typelink ast))
  (typelink-class "" ast))

;======================================================================================
;==== Import Linker Generation
;======================================================================================

(define (gen-typelink-imports imports root)
  (define (link-name-helper r name)
    (cond
      [(equal? name (first r)) (list (second r))]
      [else empty]))

  (define (link-name name)
    (cond
      [(equal? 1 (length name)) (error)] ;might not be right, but i cant find a case in java where you can import just a file (ex "import ClassA;")
      [else (define links (append-map (lambda(r) (link-name-helper r name)) root))
            (cond
              [(equal? 1 (length links)) (first links)]
              [else (error)])])) ;An import should only link to one package + class name, if theres none then the import doesnt exist, if there is more than one then there is 2 files with the same package and class declaration

  (cond
    [(empty? imports) empty]
    [else (cons (link "" (cimport-path (first imports)) (link-name (cimport-path (first imports)))) (gen-typelink-imports (rest imports) root))]))

(define (check-for-clashes links seen-so-far)
  (cond
    [(empty? links) empty]
    [(list? (memf (lambda(x) (equal? x (last (link-name (first links))))) seen-so-far)) (error)]
    [else (cons (first links) (check-for-clashes (rest links) (cons (last (link-name (first links))) seen-so-far)))]))

;======================================================================================
;==== Print Functions
;======================================================================================

(define (print-all-links all-links)
  (for-each (lambda(x) (printf "~n--- LINKS ---~n") (print-links x)) all-links) )

(define (print-links links)
  (for-each (lambda(x) (print-link x)) links))

(define (print-link link)
  (printf "~nNAME: ~a IN: ~a LINKS TO:~n~a~n" (link-name link) (link-ci link) (link-decl link)))

;======================================================================================
;==== ERROR
;======================================================================================

(define (error)
  (printf "Error~n")
  (exit 42))
    




