#lang racket

(require "machine.rkt")
(require "create-dfa.rkt")

(provide token)
(provide token-type)
(provide token-lexeme)
(provide scanner)
(provide print-token)
(provide print-tokens)
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
    [(debug-mode) (printf "~a : ~a~n" (token-type token) (token-lexeme token))]
    [else (printf "")]))
  
(define (print-tokens tokens)
  (cond
    [(debug-mode)
      (printf "====== Scanned Tokens ======~n")
      (for-each (lambda (x) (print-token x)) tokens)]
    [else (printf "")]))

;==============================================================================================
;==== Scanner Functions
;==============================================================================================

(define (ascii? c)
  (and (>= (char->integer c) 0)
       (<  (char->integer c) 128)))

(define (scanner m cl)
  (letrec ([create-token (lambda (state lex-cl)
                             (match (second (get-md m state))
                               ['decimal-lit (let ([lex (string->number (if (member (first lex-cl) '(#\l #\L))
                                                                            (list->string (reverse (rest lex-cl)))
                                                                            (list->string (reverse lex-cl))))])
                                               (cond 
                                                 [(or (> lex 2147483647) (< lex -2147483648) (not (exact-integer? lex)))
                                                  (error "Invalid integer")]
                                                 [else (token 'decimal-lit lex)]))]
                               [x (token x (list->string (reverse lex-cl)))]))]
           [get-token (lambda (cl state lexeme ccp)
                        (cond 
                          [(empty? cl) (cond
                                         [(empty? lexeme) (ccp (list (token 'EOF "") cl))]
                                         [(empty? state)  (ccp null)]
                                         [(is-state-accepting m (first state)) (ccp (list (create-token (first state) lexeme) cl))]
                                         [else (ccp null)])]
                          [(empty? state) (ccp null)]
                          [(not (ascii? (first cl))) (error "Invalid Character!")]
                          [(is-state-accepting m (first state)) (let ([tok (call/cc (lambda (cc) (get-token (rest cl) (process-char m (first state) (first cl)) (cons (first cl) lexeme) cc)))])
                                                                  (cond
                                                                    [(and (null? tok) (empty? lexeme)) (error "invalid char")]
                                                                    [(null? tok) (ccp (list (create-token (first state) lexeme) cl))]
                                                                    [(pair? tok) (ccp tok)]))]
                          [else (get-token (rest cl) (process-char m (first state) (first cl)) (cons (first cl) lexeme) ccp)]))]
           [scan (lambda (cl)
                   (cond [(empty? cl) empty]
                         [else (let ([tok (call/cc (lambda (cc) (get-token cl (list (machine-start m)) empty cc)))]) 
                                 (if (null? tok)
                                     (error "invalid token")
                                     (cons (first tok) (scan (second tok)))))]))]) 
    (with-handlers ([exn:fail? (lambda (exn) #f)])
                   (filter-not (lambda (x) (or (equal? 'whitespace (token-type x)) (equal? 'comment (token-type x)))) 
                               (scan cl)))))