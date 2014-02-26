#lang racket

(require "machine.rkt")

(provide token)
(provide token-type)
(provide token-lexeme)
(provide scanner)
(provide print-token)
(provide print-tokens)

(struct token (type lexeme) #:transparent)

;==============================================================================================
;==== Print Functions
;==============================================================================================

(define (print-token . tokens) 
  (for ([tok tokens]) (printf "~a : ~a~n" (token-type tok) (token-lexeme tok))))

(define (print-tokens token-list)
  (apply print-token token-list))

;==============================================================================================
;==== Scanner Functions
;==============================================================================================
(define (in-range? low high c)
  (and (>= (char->integer c) low)
       (< (char->integer c) high)))

(define (ascii? c) (in-range? 0 128 c))
(define (num-oct? c) (in-range? (char->integer #\0) (char->integer #\8) c))
(define (num-0to3? c) (in-range? (char->integer #\0) (char->integer #\4) c))

;(: escape-chars : (Listof Char) -> (Listof Char))
(define (escape-chars cl)
  ;(: match-octal : Nat (Listof Char) -> (Listof Char))
  (define (match-octal pos cl)
    (cond [(empty? cl) empty]
          [(and (= pos 2) (num-0to3? (first cl))) (cons (first cl) (match-octal 2 (rest cl)))]
          [(and (> pos 0) (num-oct? (first cl))) (cons (first cl) (match-octal (sub1 pos) (rest cl)))]
          [else empty]))
  ;(: list->octal : (Listof Char) -> Nat)
  (define (list->octal cl)
    (integer->char (string->number (list->string (append (list #\# #\o ) cl)))))    
  (match cl
    [`()               empty]
    [`(#\\ #\b ,x ...) (cons #\backspace (escape-chars x))]
    [`(#\\ #\t ,x ...) (cons #\tab (escape-chars x))]
    [`(#\\ #\n ,x ...) (cons #\newline (escape-chars x))]
    [`(#\\ #\f ,x ...) (cons #\page (escape-chars x))]
    [`(#\\ #\r ,x ...) (cons #\return (escape-chars x))]
    [`(#\\ #\" ,x ...) (cons #\" (escape-chars x))]
    [`(#\\ #\' ,x ...) (cons #\' (escape-chars x))]
    [`(#\\ #\\ ,x ...) (cons #\\ (escape-chars x))]
    [`(#\\ ,x ...)     (let* ([octal (match-octal 2 x)]
                              [value (list->octal octal)])
                         (cons value (escape-chars (list-tail x (length octal)))))]
    [x (cons (first x) (escape-chars (rest x)))]))


(define (convert-to-long original lex-cl)
  (if (member (first lex-cl) '(#\l #\L))
      (token 'long-lit (list->string (reverse (rest lex-cl))))
      (token original (list->string (reverse lex-cl)))))

(define (scanner m cl)
  (letrec ([create-token (lambda (state lex-cl)
                             (match (second (get-md m state))
                               ['decimal-lit (convert-to-long 'decimal-lit lex-cl)]
                               ['octal-lit (convert-to-long 'octal-lit lex-cl)]
                               ['hex-lit (convert-to-long 'hex-lit lex-cl)]
                               ['char-lit (token 'char-lit (list->string (escape-chars (reverse lex-cl))))]
                               ['string-lit (token 'string-lit (list->string (escape-chars (reverse lex-cl))))]
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
