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


;;expand-parenthesis : (Listof Char) (Listof Char) Number -> (Listof (Listof Char))

(define [expand-parenthesis L acc N]
  (cond 
    [(= N 0) (cons (foldl cons empty (rest acc)) (cons L empty))]
    [(empty? L) (error "Unbalanced Parentheses!")]
    [(char=? (first L) #\() (expand-parenthesis (rest L) (cons (first L) acc) (+ N 1))]
    [(char=? (first L) #\)) (expand-parenthesis (rest L) (cons (first L) acc) (- N 1))]
    [else (expand-parenthesis (rest L) (cons (first L) acc) N)]))

;;(: list->regex : (Listof Char) (union concatenation Char k-star alternation) -> (union concatenation Char k-star alternation))
(define (list->regex lst R)
  (cond
    [(empty? lst) R]
    [(char=? (first lst) #\\) (list->regex (rest (rest lst)) (concatenation R (first (rest lst))))]
    [(char=? (first lst) #\() 
     ((lambda (P)
        ((lambda (Q)
           (list->regex (first (rest P)) Q))
        
         (concatenation R (list->regex (first P) (empty-regex)))))
        (expand-parenthesis (rest lst) empty 1))]
    [(char=? (first lst) #\*) (concatenation (k-star R) (list->regex (rest lst) (empty-regex)))]
    [(char=? (first lst) #\|) 
     ((lambda (S)
       (cond
         [(empty-regex? S) R]
         ;[(concatenation? S) (concatenation (alternation R (concatenation-left S)) (concatenation-right S))]
         [else (alternation R S)])) 
      (list->regex (rest lst) (empty-regex)))]
     
     
    [else (list->regex (rest lst) (concatenation R (first lst)))]))

(print-machine (copy-machine (nfa->dfa (regex->machine (list->regex (string->list "grey") (empty-regex))))))
(expand-parenthesis (string->list "a|e)y") empty 1)
(print-machine (copy-machine (nfa->dfa (regex->machine (list->regex (string->list "gr(a|e)y") (empty-regex))))))

(print-machine (copy-machine (nfa->dfa (regex->machine (list->regex (string->list "a*") (empty-regex))))))



(print-machine (copy-machine (nfa->dfa (regex->machine (concatenation #\g (concatenation #\r (concatenation (k-star (alternation #\a #\e)) #\y)))))))
(print-machine (copy-machine (nfa->dfa (regex->machine (list->regex (string->list "(1|2|3|4|5|6|7|8|9)(0|1|2|3|4|5|6|7|8|9)*") (empty-regex))))))