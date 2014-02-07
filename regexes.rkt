#lang racket

(require "machine.rkt")
(require "expand-parenthesis.rkt")
(define empty-string #\~)
(define lookup-string #\#)
(provide string->machine)
;;(struct : empty-regex ())
(struct empty-regex ())

;;( struct : concatenation ([left : (union concatenation Char k-star alternation)] [right : (Union concatenation Char k-star alternation)]))
(struct concatenation (left right))

;;( struct : alternation ([options (Listof (union concatenation Char k-star alternation))]))
(struct alternation (options))

;;( struct : k-star ([body (union concatenation Char k-star alternation)]))
(struct k-star (body))

;;(: regex->machine  : (union concatenation Char k-star alternation) -> machine)
;;
(define [regex->machine R]
  (cond
    [(empty-regex? R) (m-only-epsilon)]
    [(char? R) (m-single-char R)]
    [(concatenation? R) (concat (list (regex->machine (concatenation-left R)) (regex->machine (concatenation-right R))))]
    [(alternation? R) (union (map regex->machine (alternation-options R)))]
    [(k-star? R) (kleene-star (regex->machine (k-star-body R)))]
    [else (error "Not a regular expression")]))




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
         ;[(empty-regex? S) (cons R empty)]
         [(alternation? S) (alternation (cons R (alternation-options S)))]
         [else (alternation (cons R (cons S empty)))])) 
      (list->regex (rest lst) (empty-regex)))]
     
    [(char=? (first lst) empty-string) (list->regex (rest lst) (concatenation R (empty-regex)))]
    
     
    [else (list->regex (rest lst) (concatenation R (first lst)))]))

;(print-machine (copy-machine (nfa->dfa (regex->machine (list->regex (string->list "grey") (empty-regex))))))
;(expand-parenthesis (string->list "a|e)y") empty 1)
;(print-machine (copy-machine (nfa->dfa (regex->machine (list->regex (string->list "gr(a|e)y") (empty-regex))))))

;(print-machine (copy-machine (nfa->dfa (regex->machine (list->regex (string->list "a*") (empty-regex))))))

(define (string->machine S token-name)
  (let ([machine-1 (copy-machine (regex->machine (list->regex (string->list S) (empty-regex))))])
    (nfa->dfa (machine 
     (machine-states machine-1)
     (machine-start machine-1)
     (machine-accepting machine-1)
     (machine-transitions machine-1)
     (map (lambda (x) (list x token-name)) (machine-accepting machine-1)))))) 
                        


;(print-machine (copy-machine (nfa->dfa (regex->machine (concatenation #\g (concatenation #\r (concatenation (k-star (alternation '(#\a #\e))) #\y)))))))
;(print-machine (copy-machine (nfa->dfa (regex->machine (list->regex (string->list "(1|2|3|4|5|6|7|8|9)") (empty-regex))))))
;(print-machine (copy-machine (nfa->dfa (regex->machine (list->regex (string->list "(1|2|3|4|5|6|7|8|9)*") (empty-regex))))))

;(print-machine (string->machine "((0|1|2|3|4|5|6|7|8|9)*).((0|1|2|3|4|5|6|7|8|9)*)(((E|e)(+|-|~)(0|1|2|3|4|5|6|7|8|9)((0|1|2|3|4|5|6|7|8|9)*))|~)(f|F|d|D|~)"))

;(print-machine (string->machine "/\\*(a*)\\*/"))
;(print-machine (string->machine "a|~"))
;(print-machine	(string->machine "((0|1|2|3|4|5|6|7|8|9)((0|1|2|3|4|5|6|7|8|9)*).((0|1|2|3|4|5|6|7|8|9)*)(((E|e)(+|-|~)(0|1|2|3|4|5|6|7|8|9)((0|1|2|3|4|5|6|7|8|9)*))|~)(F|f|D|d|~))|((0|1|2|3|4|5|6|7|8|9)((0|1|2|3|4|5|6|7|8|9)*)(e|E)(+|-|~)(0|1|2|3|4|5|6|7|8|9)((0|1|2|3|4|5|6|7|8|9)*)(F|f|d|D|~))|(.(0|1|2|3|4|5|6|7|8|9)((0|1|2|3|4|5|6|7|8|9)*)(((E|e)(+|-|~)(0|1|2|3|4|5|6|7|8|9)((0|1|2|3|4|5|6|7|8|9)*))|~)(F|f|D|d|~))|((0|1|2|3|4|5|6|7|8|9)((0|1|2|3|4|5|6|7|8|9)*)(((E|e)(+|-|~)(0|1|2|3|4|5|6|7|8|9)((0|1|2|3|4|5|6|7|8|9)*))|~)(F|f|D|d))"))


