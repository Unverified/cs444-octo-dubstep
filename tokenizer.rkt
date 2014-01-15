#lang racket

(require racket/include)
(include "token-state-handler.rkt")

(struct token (type lexeme))

(define (tokenize char-list state lexeme)
  (cond
    [(empty? char-list) empty]
    [(not (equal? (get-transition state (first char-list)) STATE_NONE)) (tokenize (rest char-list) (get-transition state (first char-list)) (string-append (make-string 1 (first char-list)) lexeme))]
    [(not (equal? (get-state-type state) TOKEN_WHITESPACE)) (cons (get-state-type state) (tokenize (rest char-list) (get-transition STATE_START (first char-list)) (make-string 1 (first char-list))))]
    [else (tokenize (rest char-list) (get-transition STATE_START (first char-list)) (make-string 1 (first char-list)))]))

;(token (get-state-type state) lexeme)

(define file "input.txt")
(define clist (string->list (file->string file)))

(tokenize clist STATE_START "")