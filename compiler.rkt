#lang racket

(require "parser.rkt")			;needed for parser
(require "scanner.rkt")
(require "weeder.rkt")			;needed for weeding
(require "lr-dfa.rkt")			;needed for rule
(require "create-dfa.rkt")

;==============================================================================================
;==== Parse Command Line
;==============================================================================================

;Command line debug flag
(define debug-mode (make-parameter #f))

;Parse the command line arguments and return the file to compile
(define get-file ;"tests/in/classnamegood")
  (command-line
    #:program "compiler"
    #:once-each
    [("-d" "--debug") "Print debug statements" (debug-mode #t)]
    #:args (file)
    file))

;Get the file we want to compile
(define file-to-compile get-file)

;Dont know if this is the best way to pass the debug flag
(parser-set-debug-mode debug-mode)
(scanner-set-debug-mode debug-mode)
(weeder-set-debug-mode debug-mode)

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
(define (run-weeder filename AST)
  (cond
    [(weeder filename AST) (compiled)]
    [else (error)]))


;==============================================================================================
;==== Execution
;==============================================================================================
(define (remove-dot-java filename)
  (first (regexp-split #px"\\.java" filename)))

(define (get-file-name filepath)
  (last (regexp-split #px"/" filepath)))

;Get the input as a list of chars
(define clist (string->list (file->string file-to-compile)))

(run-weeder (remove-dot-java (get-file-name file-to-compile)) (run-parser (run-scanner clist)))

