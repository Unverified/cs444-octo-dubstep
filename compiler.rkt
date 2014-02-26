#lang racket

(require "parser.rkt")			;needed for parser
(require "scanner.rkt")
(require "weeder.rkt")			;needed for weeding
(require "lr-dfa.rkt")			;needed for rule
(require "create-dfa.rkt")
(require "ast-tree.rkt")
(require "enviroments.rkt")

;==============================================================================================
;==== Parse Command Line
;==============================================================================================

;Get all the files from the command line
(define files-to-compile (vector->list (current-command-line-arguments)))

;==============================================================================================
;==== Compiler Results
;==============================================================================================

;Call this when the compiler found an error in the joos1w program. This will print "Error" and then exit
(define (error)
  (printf "Error~n")
  (exit 42))

;Call this when the compiler successfully compiled the program. Tis will print "Compiled" and exit
(define (compiled)
  (printf "Compiled~n")
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
  (cond
    [(weeder filename parse-tree) (parse->ast (find-tree 'S parse-tree))]
    [else (error)]))

;==============================================================================================
;==== Print
;==============================================================================================

(define (print-ast ast)
  (printf "============ AST ============~n~a~n~n" ast))

(define (print-asts asts files)
  (for-each (lambda(ast) (print-ast ast)) asts))

;==============================================================================================
;==== Execution
;==============================================================================================

(define (remove-dot-java filename)
  (first (regexp-split #px"\\.java" filename)))

(define (get-file-name filepath)
  (last (regexp-split #px"/" filepath)))

(define (parse-file file)
  (define clist (string->list (file->string file)))
  (define parse-tree (run-parser (run-scanner clist)))
  (run-weeder (remove-dot-java (get-file-name file)) parse-tree))

(define (parse-files files)
  (cond
    [(empty? files) empty]
    [else (cons (parse-file (first files)) (parse-files (rest files)))]))

(define asts (parse-files files-to-compile))

(print-asts asts files-to-compile)

(printf "============== Environments ==============~n")
(print-envs (gen-root-env asts))
