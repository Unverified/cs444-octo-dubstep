#lang racket

(require "token-state-handler.rkt")

(provide token)
(provide token-type)
(provide token-lexeme)
(provide scanner)
(provide print-token)
(provide scanner-set-debug-mode)

(struct token (type lexeme))

;==============================================================================================
;==== Debug
;==============================================================================================

(define debug-mode #t)

(define (scanner-set-debug-mode mode)
  (set! debug-mode mode))

;==============================================================================================
;==== Print Functions
;==============================================================================================

(define (print-token token) 
  (cond
    [debug-mode (printf "~a : ~a~n" (token-type token) (token-lexeme token))]
    [else (printf "")]))
  
(define (print-tokens tokens)
  (cond
    [debug-mode
      (printf "====== Scanned Tokens ======~n")
      (for-each (lambda (x) (print-token x)) tokens)]
    [else (printf "")]))

;==============================================================================================
;==== Scanner Functions
;==============================================================================================

(define (scanner char-list state lexeme)
  (define (scan char-list state lexeme)
    (cond
      [(empty? char-list) empty]
      [(not (equal? (get-transition state (first char-list)) STATE_NONE)) 
          (scan (rest char-list) (get-transition state (first char-list)) (string-append lexeme (make-string 1 (first char-list))))]
      [(not (equal? (get-state-type state) TOKEN_WHITESPACE)) 
          (cons (token (get-state-type state) lexeme)
                (scan (rest char-list) (get-transition STATE_START (first char-list)) (make-string 1 (first char-list))))]
      [else (scan (rest char-list) (get-transition STATE_START (first char-list)) (make-string 1 (first char-list)))]))

  (define tokens (scan char-list state lexeme))

  (print-tokens tokens)

  (define result (memf (lambda (token) (equal? TOKEN_NONE (token-type token))) tokens))
  (cond
    [(list? result) #f]
    [else tokens]))
  
