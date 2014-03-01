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
(define (error)
  (printf "Error~n")
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
    [(weeder filename parse-tree) (parse->ast (find-tree 'S parse-tree))]
    [else (error)]))

;==============================================================================================
;==== Execution
;==============================================================================================

(define (remove-dot-java filename)
  (first (regexp-split #px"\\.java" filename)))

(define (get-file-name filepath)
  (last (regexp-split #px"/" filepath)))

(define (add-stdlib-import ast)
  (cunit (cunit-package ast) (cunit-imports ast) (cunit-body ast)))

(define (parse-file file)
  (printf "GETTING AST FOR ~a~n" file)
  (define clist (string->list (file->string file)))
  (define parse-tree (run-parser (run-scanner clist)))
  (add-stdlib-import (run-weeder (remove-dot-java (get-file-name file)) parse-tree)))

(define (parse-files files)
  (cond
    [(empty? files) empty]
    [else (cons (parse-file (first files)) (parse-files (rest files)))]))

(define asts (parse-files files-to-compile))

(print-asts asts files-to-compile)

(printf "~n============== Environments ==============~n")
(define root (gen-root-env asts))

root

(printf "~n============== Type Linker ==============~n")
(print-all-links (gen-typelink-lists asts root))

(compiled)
