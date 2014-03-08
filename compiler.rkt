
#lang racket

(require "parser.rkt")			;needed for parser
(require "scanner.rkt")
(require "weeder.rkt")			;needed for weeding
(require "lr-dfa.rkt")			;needed for rule
(require "create-dfa.rkt")
(require "ast-tree.rkt")
(require "parse-tree.rkt")
(require "environments.rkt")
(require "type-linker.rkt")
(require "heirarchy-checker.rkt")

;==============================================================================================
;==== Parse Command Line
;==============================================================================================

;Get all the files from the command line
(define files-to-compile (vector->list (current-command-line-arguments)))
;(define files-to-compile (list "tests/in/Je_2_Locals_Overlapping_DeeplyNested.java"))

;==============================================================================================
;==== Compiler Results
;==============================================================================================

;Call this when the compiler found an error in the joos1w program. This will print "Error" and then exit
(define (error . x)
  (printf "Error: ~a~n" x)
  (exit 42))

;Call this when the compiler successfully compiled the program. This will print "Compiled" and exit
(define (compiled)
  (printf "Compiled!~n")
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
    [(weeder filename parse-tree) (define ast (clean-ast (parse->ast (find-tree 'S parse-tree))))
                                  (printf "============= AST ==============~n")
                                  (print-ast ast "")
                                  ast]
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
  (cunit (cunit-package ast) (remove-duplicates (cons (pimport (list "java" "lang")) (cunit-imports ast)) (lambda(x y) (same-imports x y))) (cunit-body ast)))

(define (parse-file file)
  (printf "GETTING AST FOR ~a~n" file)
  (define clist (string->list (file->string file)))
  (define parse-tree (run-parser (run-scanner clist)))
  (do-import-stuff (run-weeder (remove-dot-java (get-file-name file)) parse-tree)))

(define asts (append (map parse-file files-to-compile) (all-stdlib-asts)))
;(define asts (map parse-file files-to-compile))

(printf "~n============== PRINTING ASTS ==============~n")
(print-asts asts files-to-compile)
(define names (map c-unit-name asts))

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


(printf "~n============== Heirarchy Checker ==========~n")
(define ref-asts (map (lambda(ast rootenv) (list (roote-id (second rootenv)) ast)) asts rootenvs))
(define ref-all-links (map (lambda(links rootenv) (list (roote-id (second rootenv)) links)) all-links rootenvs))

(define full-envs (check-heirarchies ref-asts ref-all-links))

(with-handlers ([exn:fail? (lambda (exn) (begin (printf "~a" (exn-message exn))
                                                             (error)))])
  (for-each (lambda (x y) (envs-print x) (print-ast y "") (va x y)) full-envs asts))

(compiled)

