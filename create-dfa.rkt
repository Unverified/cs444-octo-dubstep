#lang racket

(require "regexes.rkt")
(require "token.rkt")
(require "machine.rkt")

;token-exps
;(assoc 'char-lit token-exps)
;(print-machine (string->machine (second (assoc 'char-lit token-exps)) 'char-lit))
;(print-machine (string->machine (second (assoc 'string-lit token-exps))))
;(print-machine (string->machine (second (assoc 'comment-lit-1 token-exps))))
;(print-machine (string->machine (second (assoc 'comment-lit-2 token-exps))))


(define all-tokens-regexes (append keywords operators literals separators))
(opt (union (map (lambda (x) (string->machine (first (rest x)) (first x))) all-tokens-regexes)))
