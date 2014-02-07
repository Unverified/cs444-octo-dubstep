#lang racket

(require "regexes.rkt")
(require "token.rkt")
(require "machine.rkt")

;token-exps
;(assoc 'char-lit token-exps)
;(print-machine (string->machine (second (assoc 'char-lit token-exps))))
;(print-machine (string->machine (second (assoc 'string-lit token-exps))))
;(print-machine (string->machine (second (assoc 'comment-lit-1 token-exps))))
(print-machine (string->machine (second (assoc 'comment-lit-2 token-exps))))


(define all-tokens-regexes (map second (map (lambda (x) (assoc x token-exps)) 
                                            (append
                                             (map first keywords)
                                             (map first operators) 
                                             (map first literals)
                                             (map first separators)))))

(define all-tokens-machine (union (map string->machine all-tokens-regexes)))

