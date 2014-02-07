#lang racket

(require "token-state-handler.rkt")

(provide token)
(provide token-type)
(provide token-lexeme)
(provide scanner)
(provide print-token)
(provide scanner-set-debug-mode)

(struct token (type lexeme) #:transparent)
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

(define (ascii? c)
  (and (>= (char->integer c) 0)
       (<  (char->integer c) 128)))

(define (scanner cl)
  (letrec ([create-token (lambda (state lex-cl)
                           (token (get-state-type state) (list->string (reverse lex-cl))))]
           [get-token (lambda (cl state lexeme ccp)
                        (cond 
                          [(and (empty? cl) (empty? lexeme))           (ccp (list (token 'EOF "") cl))]
                          [(and (empty? cl) (equal? state STATE_NONE)) (ccp null)]
                          [(empty? cl)                                 (ccp (list (create-token state lexeme) cl))]
                          [(not (ascii? (first cl)))                   (error "Invalid Character!")]
                          [(not (equal? state STATE_NONE))             (let ([tok (call/cc (lambda (cc) (get-token (rest cl) (get-transition state (first cl)) (cons (first cl) lexeme) cc)))])
                                                                         (cond
                                                                           [(null? tok) (ccp (list (create-token state lexeme) cl))]
                                                                           [(pair? tok) (ccp tok)]))]
                          [else (ccp null)]))]
           [scan (lambda (cl)
                   (cond [(empty? cl) empty]
                         [else (let ([tok (call/cc (lambda (cc) (get-token cl STATE_START empty cc)))]) (cons (first tok) (scanner (second tok))))]))])
    (filter-not (lambda (x) (or (equal? 'WHITESPACE (token-type x)) (equal? 'COMMENT (token-type x)))) 
                (scan cl))))