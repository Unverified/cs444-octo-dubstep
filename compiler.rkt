#lang racket

(require "parser.rkt")			;needed for parser
(require "scanner.rkt")			;needed for scan, token, token-type, token-lexeme
(require "token-state-handler.rkt")	;needed for START_STATE
(require "lr-dfa.rkt")			;needed for rule

;==============================================================================================
;==== Parse Command Line
;==============================================================================================

;Command line debug flag
(define debug-mode (make-parameter #f))

;Parse the command line arguments and return the file to compile
(define get-file ;"tests/in/input")
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
  (define tokens (scanner chars STATE_START ""))
  (cond
    [(list? tokens) (append tokens (list (token '$ "$")))]
    [else (error)]))
  
;==============================================================================================
;==== Parser
;==============================================================================================

;Runs the parser, checks if the parser successfully parsed the tokens given. If it did it will 
;call compiled (for now), else call error
(define (run-parser tokens)
  (define result-stack (parser tokens))
  (cond
    [(empty? result-stack) (error)]
    [(equal? (first (parser-stack-token result-stack)) (rule-lhs start-rule)) (compiled)]
    [else (error)]))

;==============================================================================================
;==== Execution
;==============================================================================================

;Get the input as a list of chars
(define clist (string->list (file->string file-to-compile)))

(run-parser (run-scanner clist))

