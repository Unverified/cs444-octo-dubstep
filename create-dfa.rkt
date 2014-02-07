#lang racket

(require "regexes.rkt")
(require "token.rkt")
(require "machine.rkt")


(define all-tokens-regexes (append keywords operators literals separators))
(define all-tokens-machine (copy-machine (nfa->dfa (union (map (lambda (x) (copy-machine (opt (string->machine (first (rest x)) (first x))))) token-exps)))))

(write all-tokens-machine)

