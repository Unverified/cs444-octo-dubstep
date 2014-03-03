#lang racket

(require "parser.rkt")			;needed for parser
(require "scanner.rkt")
(require "weeder.rkt")			;needed for weeding
(require "lr-dfa.rkt")			;needed for rule
(require "create-dfa.rkt")
(require "ast-tree.rkt")
(require "parse-tree.rkt")
(require "enviroments.rkt")
(require "type-linker.rkt")
;(require "heirarchy.rkt")

;==============================================================================================
;==== Parse Command Line
;==============================================================================================

;Get all the files from the command line
(define files-to-compile (vector->list (current-command-line-arguments)))
;(define files-to-compile (list "tests/test1.java"))

;==============================================================================================
;==== Compiler Results
;==============================================================================================

;Call this when the compiler found an error in the joos1w program. This will print "Error" and then exit
(define (error . x)
  (printf "Error: ~a~n" x)
  (exit 42))

;Call this when the compiler successfully compiled the program. This will print "Compiled" and exit
(define (compiled)
  (printf "Compiled~n")
  (exit 0))

;==============================================================================================
;==== Scanner
;==============================================================================================

;Runs the scanner, checks if the scanner properly scanned the tokens, then either calles error or
;returns the tokens
(define (run-scanner chars)
  (printf "RUNNING SCANNER~n")
  (define tokens (scanner (all-tokens-machine) chars))
  (cond
    [(list? tokens) tokens]
    [else (error)]))
  
;==============================================================================================
;==== Parser
;==============================================================================================

(define (check-node sym node)
  (cond
    [(leafnode? node) (equal? sym (token-type (leafnode-token node)))]
    [else (equal? sym (node-sym node))]))

(define (check-node-stack node-stack)
  (define valid-stack (rule-rhs start-rule))
  (cond
    [(not (equal? (length node-stack) (length valid-stack))) #f]
    [else (andmap (lambda (valid-sym tree) (check-node valid-sym (tree-node tree))) valid-stack node-stack)]))

;Runs the parser, checks if the parser successfully parsed the tokens given. If it did it will 
;call compiled (for now), else call error
(define (run-parser tokens)
  (printf "RUNNING PARSER~n")
  (define node-stack (parser tokens))
  (cond
    [(empty? node-stack) (error)]
    [(check-node-stack node-stack) (list-ref node-stack 1)]
    [else (error)]))
  
;==============================================================================================
;==== Weeder
;==============================================================================================

;Runs the parser, checks if the parser successfully parsed the tokens given. If it did it will 
;call compiled (for now), else call error
(define (run-weeder filename parse-tree)
  (printf "RUNNING WEEDER~n")
  (cond
    [(weeder filename parse-tree) (define ast (parse->ast (find-tree 'S parse-tree)))
                                  (printf "============= AST ==============~n")
                                  (print-ast ast "")
                                  (clean-ast ast)]
    [else (error)]))

;==============================================================================================
;==== Get STDLIB asts
;==============================================================================================

(define stdlib-files (list "stdlib/java/io/OutputStream.java" "stdlib/java/io/PrintStream.java" "stdlib/java/io/Serializable.java" "stdlib/java/lang/Boolean.java" "stdlib/java/lang/Byte.java" "stdlib/java/lang/Character.java" "stdlib/java/lang/Class.java" "stdlib/java/lang/Cloneable.java" "stdlib/java/lang/Integer.java" "stdlib/java/lang/Number.java" "stdlib/java/lang/Object.java" "stdlib/java/lang/Short.java" "stdlib/java/lang/String.java" "stdlib/java/lang/System.java" "stdlib/java/util/Arrays.java"))
(define stdlib-file (string->path "stdlib.asts"))
(define (all-stdlib-asts) 
  (if (file-exists? stdlib-file)
      (call-with-input-file stdlib-file read)
      (let ([stdlib (map parse-file stdlib-files)]
            [out (open-output-file stdlib-file)])
        (write stdlib out)
        (close-output-port out)
        stdlib)))

;==============================================================================================
;==== Execution
;==============================================================================================

(define (remove-dot-java filename)
  (first (regexp-split #px"\\.java" filename)))

(define (get-file-name filepath)
  (last (regexp-split #px"/" filepath)))

(define (do-import-stuff ast)
  (define (same-imports x y)
    (match (list x y)
      [(or `(,(cimport x) ,(cimport y))
           `(,(pimport x) ,(pimport y))) (equal? x y)]
      [else #f]))
  (printf "DOING IMPORT STUFF~n")
  (cunit (cunit-package ast) (remove-duplicates (cunit-imports ast) (lambda(x y) (same-imports x y))) (cunit-body ast)))
  ;(cunit (cunit-package ast) (remove-duplicates (cons (pimport (list "java" "lang")) (cunit-imports ast)) (lambda(x y) (same-imports x y))) (cunit-body ast)))

(define (parse-file file)
  (printf "GETTING AST FOR ~a~n" file)
  (define clist (string->list (file->string file)))
  (define parse-tree (run-parser (run-scanner clist)))
  (do-import-stuff (run-weeder (remove-dot-java (get-file-name file)) parse-tree)))

(define asts (append (map parse-file files-to-compile))) ;(all-stdlib-asts)))

(printf "~n============== PRINTING ASTS ==============~n")

(print-asts asts files-to-compile)

(printf "~n============== Environments ==============~n")

(define rootenvs (with-handlers ([exn:fail? (lambda (exn) (begin (printf "~a" (exn-message exn))
                                                             (error)))])
               (gen-root-env asts)))

(for-each (lambda (x) 
            (printf "~a~n============================~n" (first x))
            (envs-print (roote-env (second x)))) rootenvs)

(printf "~n============== Type Linker ==============~n")
(define all-links (gen-typelink-lists asts rootenvs))
(print-all-links all-links rootenvs)















(define ref-asts (map (lambda(ast rootenv) (list (roote-id (second rootenv)) ast)) asts rootenvs))
(define ref-all-links (map (lambda(links rootenv) (list (roote-id (second rootenv)) links)) all-links rootenvs))

(define (check-for-duplication l parents)
  (cond 
    [(list? (member (link-full (second l)) parents)) (printf "Either a class extends looped or you are implementing an interface twice.~n") (error)]
    [else l]))

(define (get-ast-extends ast)
  (define extends (get-extends ast))
  (define java-lang-Object (list "java" "lang" "Object")) 
  (cond
;    [(and (empty? extends) (not (equal? (c-unit-name ast) java-lang-Object))) java-lang-Object]
    [else extends]))

(define (get-env p)
  (roote-env (link-env (second p))))

(define (get-full typename links)
  (link-full (second (assoc typename links))))

(define (check-single-field field derived-fields base-env derived-env)
  (cond
    [(pair? (assoc (first field) derived-fields))
     (let* ([val (second (assoc (first field) (envs-vars derived-env)))]
            [scope-1 (var-scope (eval-ast val))]
            [scope-2 (var-scope (eval-ast (second (assoc (first field) (envs-vars base-env)))))])
       (cond [(equal? scope-1 scope-2) empty]
             [else (printf "Field ~a has different permission from its parent~n" (first field))
                   (exit 42)]))]
    [else empty]))


(define (check-heirarchies asts all-links)
  (define (get-linked-ast l)
    (define rootenv (link-env (second l)))
    (define id (roote-id rootenv))
    (second (assoc id asts)))
  
  (define (get-linked-links l)
    (define rootenv (link-env (second l)))
    (define id (roote-id rootenv))
    (second (assoc id all-links)))

  (define (check-class-link l parent-extds)
    (cond
      [(false? l) l]
      [(not (is-class (get-linked-ast l))) (error "Must extend a class.")]
      [else (check-for-duplication l parent-extds)]))

  (define (check-interface-links ls seen-so-far)
    (define (check-interface-link l)
      (cond
        [(false? l) l]
        [(not (is-interface (get-linked-ast l))) (error "Must implement an interface.")]
        [else (check-for-duplication l seen-so-far)]))
    (cond
      [(empty? ls) empty]
      [else (cons (check-interface-link (first ls)) (check-interface-links (rest ls) (cons (link-full (second (first ls))) seen-so-far)))]))

  (define (get-class-stuff class-link parent-extds)
    (printf "--- Getting class stuff for: ~a~n" class-link)
    (cond
      [(false? class-link) env-empty]
      [else (get-class-heriarchy (get-linked-ast class-link) (get-linked-links class-link) parent-extds)]))

  (define (get-interface-stuff interface-link parent-impls)
    (printf "--- Getting interface stuff for: ~a~n" interface-link)
    (cond
     [(false? interface-link) env-empty]
      [else (get-interface-heriarchy (get-linked-ast interface-link) (get-linked-links interface-link) parent-impls)]))

  (define (get-class-heriarchy ast links extds)
    (printf "CHECKING CLASS HEIR FOR: ~a~n" (c-unit-name ast))
    (define parent-extds (cons (c-unit-name ast) extds))

    (define extends (get-ast-extends ast))
    (define implements (get-implements ast))

    (printf "--- EXTENDS: ~a~n" extends)
    (printf "--- IMPLEMENTS: ~a~n" implements)

    (define class-link (check-class-link (assoc extends links) parent-extds))
    (define interface-links (check-interface-links (map (lambda(i) (assoc i links)) implements) empty))
    
    (define extends-env (get-class-stuff class-link parent-extds))
    (define interface-envs (map (lambda(x) (get-interface-stuff x empty)) interface-links))

    (define cur-class-env (foldr (curry combine-ci-envs links) (get-env (assoc (c-unit-name ast) links)) interface-envs))

    ; "DO STUFF HERE"    
    (map (lambda (x) (check-single-field x (envs-vars cur-class-env) cur-class-env extends-env)) (envs-vars extends-env))
    

    (combine-ci-envs links cur-class-env extends-env))


  (define (get-interface-heriarchy ast links impls)
    (printf "CHECKING INTERFACE HEIR FOR: ~a~n" (c-unit-name ast))
    (define parent-impls (cons (c-unit-name ast) impls))

    (define extends (get-extends ast))
    (printf "--- EXTENDS: ~a~n" extends)

    (define interface-links (check-interface-links (map (lambda(i) (assoc i links)) extends) parent-impls))
    (define interface-envs (map (lambda(x) (get-interface-stuff x parent-impls)) interface-links))

    (foldr (curry combine-ci-envs links) (get-env (assoc (c-unit-name ast) links)) interface-envs))

  (define (check-heirarchy ast links)
    (cond
      [(is-class ast) (print-heir (get-class-heriarchy ast links empty))]
      [(is-interface ast) (print-heir (get-interface-heriarchy ast links empty))]))

  (map (lambda(ast links) (printf "====== CHECKING HEIRARCHY FOR AST, class/interface: ~a ======~n" (c-unit-name (second ast))) (check-heirarchy (second ast) (second links))) asts all-links))
  
(define (type-ast=? links t1 t2)
  (match (list t1 t2)
    [`(,(ptype _ ta) ,(ptype _ tb)) (equal? ta tb)]
    [`(,(atype _ ta) ,(atype _ tb)) (type-ast=? links ta tb)]
    [`(,(rtype _ ta) ,(rtype _ tb)) (type-ast=? links ta tb)]
    [`((,ta ...) (,tb ...)) (equal? (get-full ta links) (get-full tb links))]
    [_ #f]))

(define (scope<? s1 s2)
  (define (get-scope-val scope)
    (define scope-order '(public protected private))
    (length (takef-right scope-order (curry symbol=? scope))))
  (< (get-scope-val s1) (get-scope-val s2)))

(define (combine-ci-envs links ienv cenv)
  (define cmethods (map first (envs-methods cenv)))
  (define imethods (map first (envs-methods ienv)))
  (define imethod-types (map (lambda (x) (list (assoc x (envs-methods cenv)) (assoc x (envs-methods ienv)))) imethods))
  (define (can-shadow? m1 m2)
    (match-let ([(method _ s1 m1 t1 _ _) m1]
                [(method _ s2 m2 t2 _ _) m2])
      (cond
        [(not (scope<? s1 s2)) (error "subclass can not lower scope")]
        [(not (type-ast=? links t1 t2)) (error "return types not equal")]
        [else #t])))
  
  (define (combine-step par env)
    (match par
      [`(,#f ,x) (env-append env (envs (list (assoc (first x) (envs-types ienv))) empty (list x) empty))]
      [`(,x ,y)  (if (can-shadow? (second x) (second y)) env (error))]))
  (foldr combine-step cenv imethod-types))

  


(define (print-heir e)
  (printf "CLASS ENV:~n")
  (envs-print e))

(printf "~n============== BLAH ==============~n")
(check-heirarchies ref-asts ref-all-links)

;(printf "~n============== Heirarchy Checker ==========~n")
;(check-heirarchy all-links)
;(compiled)
