#lang racket

(require "class-info.rkt")
(require "errorf.rkt")
(require "parser.rkt")			;needed for parser
(require "scanner.rkt")
(require "weeder.rkt")			;needed for weeding
(require "lr-dfa.rkt")			;needed for rule
(require "create-dfa.rkt")
(require "ast-tree.rkt")
(require "parse-tree.rkt")
(require "environments.rkt")
(require "type-linker.rkt")
(require "type-expr.rkt")
(require "heirarchy-checker.rkt")
(require "disambiguator.rkt")

(provide (struct-out info))

;==============================================================================================
;==== Parse Command Line
;==============================================================================================

;Get all the files from the command line
(define files-to-compile (vector->list (current-command-line-arguments)))
;(define files-to-compile (list "tests/in/a3/J2_interfaces/J2_interface.java" "tests/in/a3/J2_interfaces/Main.java"))
;(define files-to-compile (list "tests/in/a3/Je_5_ForwardReference_FieldInOwnInitializer_Direct.java"))

;==============================================================================================
;==== Compiler Results
;==============================================================================================

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
  (define tokens (scanner (all-tokens-machine) chars))
  (cond
    [(list? tokens) tokens]
    [else (c-errorf "Scanner Failed")]))
  
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
  (define node-stack (parser tokens))
  (cond
    [(empty? node-stack) (c-errorf "Parser Failed")]
    [(check-node-stack node-stack) (list-ref node-stack 1)]
    [else (c-errorf "Parser Failed")]))
  
;==============================================================================================
;==== Weeder
;==============================================================================================

;Runs the parser, checks if the parser successfully parsed the tokens given. If it did it will 
;call compiled (for now), else call error
(define (run-weeder filename parse-tree)
  (cond
    [(weeder filename parse-tree) (define ast (clean-ast (parse->ast (find-tree 'S parse-tree))))
                                  (printf "============= AST ==============~n")
                                  (print-ast ast "")
                                  ast]
    [else (c-errorf "Weeder Failed")]))

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
  (display "DONE PARSING FILE\n" (current-error-port))
  (define (same-imports x y)
    (match (list x y)
      [(or `(,(cimport x) ,(cimport y))
           `(,(pimport x) ,(pimport y))) (equal? x y)]
      [else #f]))
  (cunit (cunit-package ast) (remove-duplicates (cons (pimport (list "java" "lang")) (cunit-imports ast)) (lambda(x y) (same-imports x y))) (cunit-body ast)))

(define (parse-file file)
  (define clist (string->list (file->string file)))
  (define parse-tree (run-parser (run-scanner clist)))
  (do-import-stuff (run-weeder (remove-dot-java (get-file-name file)) parse-tree)))

(define asts (append (map parse-file files-to-compile) (all-stdlib-asts)))
;(define asts (map parse-file files-to-compile))

(printf "~n============== PRINTING ASTS ==============~n")
(print-asts asts files-to-compile)
(define names (map c-unit-name asts))

(printf "~n============== Type Linker ==============~n")
;class-info == something like (list (pair ("java" "lang" "String") (ast links)) ...)
(define links (gen-typelink-lists asts names))

(printf "~n============== Environments ==============~n")
(define class-info (map (lambda (x) (set-cinfo-env x (gen-class-envs (info-ast x)))) links))

(printf "~n============== Heirarchy Checker ==========~n")
(define class-info2 (check-heirarchies class-info)) ;alters the env in each info struct

(printf "~n=====================Local Environment Generation=========================~n")
(define class-info3 (map (lambda (cinfo)
                           (set-cinfo-ast cinfo (va (info-env cinfo) (info-ast cinfo)))) class-info2))
(for-each print-info class-info3)
class-info3

(printf "~n~n============== Disambiguator ==========~n")
(define disambig-cinfo (map (curryr disambiguate names) class-info3))


(printf "~n~n===========Type Checking==========~n")
(type-check disambig-cinfo)

(compiled)






