#lang racket

;(struct: rule ([lhs : Symbol] [rhs : (Listof Symbol)]))
(struct rule (lhs rhs))

;(struct: lritem ([dot : Integer] [rule : (Listof rule)]))
(struct lritem (dot rule))

;(struct: lrstate ([state : Symbol] [rhs : (Listof lritem)]))
(struct lrstate (state lritems))

;(: is-non-terminal : Symbol (Listof Symbol) -> Boolean)
(define (is-non-terminal sym non-terminals)
  (list? (member sym non-terminals)))

;(: get-new-lr-state-rules : lritem (Listof rule) -> (listof lritem)
;gets a list of rules where the lhs matches the non-terminal symbol right before items' dot 
(define (get-new-lr-state-rules item rules)
  (define sym (list-ref (rule-rhs (lritem-rule item)) (lritem-dot item)))
  (cond
    [(empty? rules) empty]
    [(equal? sym (rule-lhs (first rules))) (cons (lritem 0 (first rules)) (get-new-lr-state-rules item (rest rules)))]
    [else (get-new-lr-state-rules item (rest rules))]))

;(: create-lr-state : (Listof lritem) (Listof rule) (Listof Symbol) -> (Listof lritem)
;recursively creates a list of lritems. added-lhs is used to make sure we done add already added rules.
(define (create-lr-state lritems rules added-lhs)
  (define should-add-lritem (lambda (x) (not (list? (member (list-ref (rule-rhs (lritem-rule x)) (lritem-dot x)) added-lhs)))))
  (define new-lritems (append-map (lambda (x) (if (should-add-lritem x) (get-new-lr-state-rules x rules) empty)) lritems))
  (define (add-lhss items) (append added-lhs (map (lambda (x) (rule-lhs (lritem-rule x))) items)))
  (if (empty? new-lritems) lritems (append lritems (create-lr-state new-lritems rules (add-lhss new-lritems)))))

;(: parse=grammar : lritem (Listof Symbol) (Listof Symbol) (Listof rule) -> (Listof something)
;takes in an lritem and a list of terminals, non-terminals, and rules. It will return some data structure
;used to parse an lr grammar probably something of the form (state symbol action reaction) where
; - "state" is the lr grammars state we are looking at
; - "symbol" is an input symbol (eg token)
; - "action" is either 'shift or 'reduce
; - "reaction" is either the new state to go to on a shift or the rule to reduce by
(define (parse-grammar item terminals non-terminals rules)
  (define can-reduce (lambda (x) (equal? (lritem-dot x) (length (rule-rhs (lritem-rule x))))))
  (define inc-dot-pos (lambda (x) (lritem (+ 1 (lritem-dot x)) (lritem-rule x))))
  (define handle-lritem 
    (lambda (x)
      (define next-item (inc-dot-pos x))
      (cond
        [(can-reduce next-item) (cons "reduce" empty)]
        [else (cons "shift" (parse-grammar next-item terminals non-terminals rules))])))
  (append-map handle-lritem (create-lr-state (list item) rules empty)))
  
;==============================================================================================
;==== Printing
;==============================================================================================

;(: print-rule : rule -> Symbol)
(define (print-rule rule) (printf "~a -> ~a~n" (rule-lhs rule) (rule-rhs rule)))

;(: print-lritem : lritem -> Symbol)
(define (print-lritem lritem) (printf "~a : " (lritem-dot lritem)) (print-rule (lritem-rule lritem)))

;(: print-lritems : (Listof lritem) -> Symbol)
(define (print-lritems lritems) (for-each (lambda (x) (print-lritem x)) lritems))

;==============================================================================================
;==== Testing
;==============================================================================================

(define terminals (list 'a 'b))
(define non-terminals (list 'S 'E 'A 'B))
(define rules 
   (list (rule 'S '(E))
    (rule 'E '(E + a))
    (rule 'E '(B + a))
    (rule 'A '(a))
    (rule 'B '(b))))

(define startrule (lritem 0 (rule 'S (list 'E))))

;(print-lritems (create-lr-state (list startrule) rules empty))
(parse-grammar startrule terminals non-terminals rules)
