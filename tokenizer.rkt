(require racket/include)
(include "token-state-handler.rkt")

(struct token (type lexeme))

(define (print-token token) (printf "~a : ~a~n" (token-type token) (token-lexeme token))) 

(define (tokenize char-list state lexeme)
  (cond
    [(empty? char-list) empty]
    [(not (equal? (get-transition state (first char-list)) STATE_NONE)) 
        (tokenize (rest char-list) (get-transition state (first char-list)) (string-append lexeme (make-string 1 (first char-list))))]
    [(not (equal? (get-state-type state) TOKEN_WHITESPACE)) 
        (cons (token (get-state-type state) lexeme)
              (tokenize (rest char-list) (get-transition STATE_START (first char-list)) (make-string 1 (first char-list))))]
    [else (tokenize (rest char-list) (get-transition STATE_START (first char-list)) (make-string 1 (first char-list)))]))
