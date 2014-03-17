#lang racket

(require "errorf.rkt")
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
       (<  (char->integer c) high)))

(define ascii? (curry in-range? 0 128))
(define num-oct?  (curry in-range? (char->integer #\0) (char->integer #\8)))
(define num-0to3? (curry in-range? (char->integer #\0) (char->integer #\4)))

(define (convert-to-long original lexeme)
  (if (member (last lexeme) '(#\l #\L))
      (token 'long-lit (list->string (reverse (rest (reverse lexeme)))))
      (token original (list->string lexeme))))

(define (create-token tkn-sym lexeme)
  (match tkn-sym
    ['decimal-lit (convert-to-long 'decimal-lit (string->list lexeme))]
    ['octal-lit (convert-to-long 'octal-lit (string->list lexeme))]
    ['hex-lit (convert-to-long 'hex-lit (string->list lexeme))]
    ['char-lit (token 'char-lit (char->integer (first (escape-chars (string->list lexeme)))))]
    ['string-lit (token 'string-lit (list->string (escape-chars (string->list lexeme))))]
    [x (token x lexeme)]))

(define (scanner m char-list)
  (define (scan cl state lexeme)
    (let* ([next-state (if (not (empty? cl)) (process-char-hash m state (first cl)) #f)]
           [tkn-sym (if (false? next-state) (get-md-hash m state) #f)])

      (cond
        [(empty? cl) empty]
        [(not (ascii? (first cl))) (c-errorf "Invalid Character!")]
        [(symbol? next-state)
            (scan (rest cl) next-state (string-append lexeme (make-string 1 (first cl))))]
        [(not (or (equal? tkn-sym 'whitespace) (equal? tkn-sym 'comment))) 
            (cons (create-token tkn-sym lexeme)
                  (scan (rest cl) (process-char-hash m (machine-start m) (first cl)) (make-string 1 (first cl))))]
        [else (scan (rest cl) (process-char-hash m (machine-start m) (first cl)) (make-string 1 (first cl)))])))

  ;(define tokens (scan char-list (machine-start m) ""))
  ;(print-tokens tokens))
  (scan char-list (machine-start m) ""))
  
  ;(define result (memf (lambda (token) (equal? TOKEN_NONE (token-type token))) tokens))
  ;(cond
  ;  [(list? result) #f]
  ;  [else tokens]))

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
