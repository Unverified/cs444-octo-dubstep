#lang racket

(require "parser.rkt")			;needed for parser
(require "scanner.rkt")			;needed for scan, token, token-type, token-lexeme
(require "weeder.rkt")			;needed for weeding
(require "token-state-handler.rkt")	;needed for START_STATE
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

(define op-rules (list 'star 'slash 'pct 'plus 'minus 'lt 'gt 'lteq 'gteq 'eqeq 'noteq 'amp 'bar 'ampamp 'barbar 'eq))
(define lit-rules (list 'decimal-lit 'id))

(define (is-op sym)
  (list? (member sym op-rules)))

(define (find-function-args-end open-count tokens)
  (cond
    [(empty? tokens) empty]
    [(equal? open-count 0) (handle-ids tokens)]
    [(equal? 'oparen (token-type (first tokens))) (cons (first tokens) (find-function-args-end (+ open-count 1) (pre-parser (rest tokens))))]
    [(equal? 'cparen (token-type (first tokens))) (cons (first tokens) (find-function-args-end (- open-count 1) (pre-parser (rest tokens))))]
    [else (cons (first tokens) (find-function-args-end open-count (pre-parser (rest tokens))))]))

(define (handle-ids tokens)
  (cond
    [(empty? tokens) empty]
    [(equal? 'dot (token-type (first tokens))) (cons (first tokens) (handle-ids (rest tokens)))]
    [(equal? 'id (token-type (first tokens))) (cons (first tokens) (handle-ids (rest tokens)))]
    [(equal? 'this (token-type (first tokens))) (cons (first tokens) (handle-ids (rest tokens)))]
    [(equal? 'oparen (token-type (first tokens))) (cons (first tokens) (find-function-args-end 1 (rest tokens)))]
    [else (cons (token 'cparen ")") tokens)]))

(define (find-closing-paren open-count tokens)
  (cond
    [(empty? tokens) empty]
    [(equal? open-count 0) (cons (token 'cparen ")") tokens)]
    [(equal? 'oparen (token-type (first tokens))) (cons (first tokens) (find-closing-paren (+ open-count 1) (pre-parser (rest tokens))))]
    [(equal? 'cparen (token-type (first tokens))) (cons (first tokens) (find-closing-paren (- open-count 1) (pre-parser (rest tokens))))]
    [else (cons (first tokens) (find-closing-paren open-count (pre-parser (rest tokens))))]))

(define (place-cparen tokens)
  (cond
    [(empty? tokens) empty]
    [(equal? 'decimal-lit (token-type (first tokens))) (cons (first tokens) (cons (token 'cparen ")") (pre-parser (rest tokens))))]
    [(equal? 'oparen (token-type (first tokens))) (cons (first tokens) (find-closing-paren 1 (rest tokens)))]
    [(equal? 'id (token-type (first tokens))) (cons (first tokens) (handle-ids (rest tokens)))]   
))

(define (pre-parser tokens)
  (cond
    [(< (length tokens) 2) tokens]
    [(and (is-op (token-type (first tokens))) (equal? 'minus (token-type (first (rest tokens))))) 
          (cons (first tokens) (cons (token 'oparen "(") (cons (first (rest tokens)) (place-cparen (rest (rest tokens))))))]
    [else (cons (first tokens) (pre-parser (rest tokens)))]))









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
  (first (regexp-split #px"\\." filename)))

(define (get-file-name filepath)
  (last (regexp-split #px"/" filepath)))

;Get the input as a list of chars
(define clist (string->list (file->string file-to-compile)))

(run-weeder (remove-dot-java (get-file-name file-to-compile)) (run-parser (pre-parser (run-scanner clist))))

