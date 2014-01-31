#lang racket

(require "machine.rkt")

;;(struct : empty-regex ())
(struct empty-regex ())

;;( struct : concatenation ([left : (union concatenation Char k-star alternation)] [right : (Union concatenation Char k-star alternation)]))
(struct concatenation (left right))

;;( struct : alternation ([option-1 : (union concatenation Char k-star alternation)] [option-2 : (union concatenation Char k-star alternation)]))
(struct alternation (option-1 option-2))

;;( struct : k-star ([body (union concatenation Char k-star alternation)]))
(struct k-star (body))


;;(: regex->machine  : (union concatenation Char k-star alternation) -> machine)
;;
(define [regex->machine R]
  (cond
    [(empty-regex? R) (m-only-epsilon)]
    [(char? R) (m-single-char R)]
    [(concatenation? R) (concat (list (regex->machine (concatenation-left R)) (regex->machine (concatenation-right R))))]
    [(alternation? R) (union (list (regex->machine (alternation-option-1 R)) (regex->machine (alternation-option-2 R))))]
    [(k-star? R) (kleene-star (regex->machine (k-star-body R)))]
    [else (error "Not a regular expression")]))

;;(: list->regex : (Listof Char) (union concatenation Char k-star alternation) -> (union concatenation Char k-star alternation))
(define (list->regex lst R)
  (cond
    [(empty? lst) R]
    [(char=? (first lst) #\\) (list->regex (rest (rest lst)) (concatenation R (first (rest lst))))]
    [(char=? (first lst) #\() (concatenation R (list->regex (rest lst) (empty-regex)))]
    [(char=? (first lst) #\)) (list->regex (rest lst) R)]
    [(char=? (first lst) #\*) (list->regex (rest lst) (k-star R))]
    [(char=? (first lst) #\|) (alternation R (list->regex (rest lst) (empty-regex)))]
    [else (list->regex (rest lst) (concatenation R (first lst)))]))


(print-machine (copy-machine (nfa->dfa (regex->machine (list->regex (string->list "gr(a|e)y") (empty-regex))))))

(print-machine (copy-machine (nfa->dfa (regex->machine (list->regex (string->list "(1|2|3|4|5)*") (empty-regex))))))

(print-machine (copy-machine (nfa->dfa (regex->machine (list->regex (string->list "abcde") (empty-regex))))))
