#lang racket
(provide expand-parenthesis)

;;expand-parenthesis : (Listof Char) (Listof Char) Number -> (Listof (Listof Char))

(define [expand-parenthesis L acc N]
  (cond 
    [(= N 0) (cons (foldl cons empty (rest acc)) (cons L empty))]
    [(empty? L) (error "Unbalanced Parentheses!")]
    [(char=? (first L) #\() (expand-parenthesis (rest L) (cons (first L) acc) (+ N 1))]
    [(char=? (first L) #\)) (expand-parenthesis (rest L) (cons (first L) acc) (- N 1))]
    [else (expand-parenthesis (rest L) (cons (first L) acc) N)]))