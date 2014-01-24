#lang racket

;(struct: rule ([lhs : Symbol] [rhs : (Listof Symbol)]))
(struct rule (lhs rhs))

;(struct: lritem ([dot : Integer] [rule : (Listof rule)]))
(struct lritem (dot rule))

;(struct: dfa-item ([state : Integer] [input : Symbol] [action : Symbol] [reaction : Integer]))
(struct dfa-item (state input action reaction))

;(: is-non-terminal : Symbol (Listof Symbol) -> Boolean)
(define (is-non-terminal sym non-terminals)
  (list? (member sym non-terminals)))

(define (get-dot-sym item) (list-ref (rule-rhs (lritem-rule item)) (lritem-dot item)))

;(: get-new-lr-state-rules : lritem (Listof rule) -> (listof lritem)
;gets a list of rules where the lhs matches the non-terminal symbol right before items' dot 
(define (get-new-lr-state-rules item rules)
  (cond
    [(empty? rules) empty]
    [(equal? (get-dot-sym item) (rule-lhs (first rules))) (cons (lritem 0 (first rules)) (get-new-lr-state-rules item (rest rules)))]
    [else (get-new-lr-state-rules item (rest rules))]))

;(: create-lr-state : (Listof lritem) (Listof rule) (Listof Symbol) -> (Listof lritem)
;recursively creates a list of lritems. added-lhs is used to make sure we done add already added rules.
(define (create-lr-state lritems rules added-lhs)
  (define (add-lhss items) (append added-lhs (map (lambda (x) (rule-lhs (lritem-rule x))) items)))
  (define (get-rules x) (set! added-lhs (add-lhss x)) x)
  (define should-add-lritem (lambda (x) (not (list? (member (list-ref (rule-rhs (lritem-rule x)) (lritem-dot x)) added-lhs)))))
  (define new-lritems (append-map (lambda (x) (if (should-add-lritem x) (get-rules (get-new-lr-state-rules x rules)) empty)) lritems))
  (print-lritems new-lritems)
  (if (empty? new-lritems) lritems (append lritems (create-lr-state new-lritems rules added-lhs))))

(define (get-rule-pos item rules)
  (define rule (lritem-rule item))
  (cond
    [(empty? rules) 0]
    [(and (equal? (rule-lhs rule) (rule-lhs (first rules))) (equal? (rule-rhs rule) (rule-rhs (first rules)))) 0]
    [else (+ 1 (get-rule-pos item (rest rules)))]))


;(: parse=grammar : lritem (Listof Symbol) (Listof Symbol) (Listof rule) -> (Listof something)
;takes in an lritem and a list of terminals, non-terminals, and rules. It will return some data structure
;used to parse an lr grammar probably something of the form (state symbol action reaction) where
; - "state" is the lr grammars state we are looking at
; - "symbol" is an input symbol (eg token)
; - "action" is either 'shift or 'reduce
; - "reaction" is either the new state to go to on a shift or the rule to reduce by
(define (parse-grammar item state terminals non-terminals rules)
  (define can-reduce (lambda (x) (equal? (lritem-dot x) (length (rule-rhs (lritem-rule x))))))
  (define inc-dot-pos (lambda (x) (lritem (+ 1 (lritem-dot x)) (lritem-rule x))))
  (define handle-lritem 
    (lambda (x)
      (define next-item (inc-dot-pos x))
      (define next-state (gensym))
      (cond
        [(can-reduce next-item) (cons (dfa-item state (get-dot-sym x) 'reduce (get-rule-pos x rules)) empty)]
        [else (cons (dfa-item state (get-dot-sym x) 'shift next-state) (parse-grammar next-item next-state terminals non-terminals rules))])))
  (append-map handle-lritem (create-lr-state (list item) rules empty)))

(define (replace-state new-state old-state dfa-items)
  (append-map (lambda (x) (if (equal? old-state (dfa-item-state x)) (dfa-item new-state (dfa-item-input x) (dfa-item-action x) (dfa-item-reaction x)) x)) dfa-items))

(define (merge-state dfa-item dfa-items)
  (append-map (lambda (x) (if (and (equal? (dfa-item-state dfa-item) (dfa-item-state x)) (equal? (dfa-item-input dfa-item) (dfa-item-input x))) (replace-state (dfa-item-state dfa-item) (dfa-item-state x) dfa-items) x)) dfa-items))

(define (merge-states dfa-items)
  (cond
    [(equal? 1 (length dfa-items)) (first dfa-items)]
    [else (append (merge-state (first dfa-items) (rest dfa-items)) (merge-states (rest dfa-items)))]))
    
;==============================================================================================
;==== Printing
;==============================================================================================

;(: print-rule : rule -> Symbol)
(define (print-rule rule) (printf "~a -> ~a~n" (rule-lhs rule) (rule-rhs rule)))

;(: print-lritem : lritem -> Symbol)
(define (print-lritem lritem) (printf "~a : " (lritem-dot lritem)) (print-rule (lritem-rule lritem)))

;(: print-lritems : (Listof lritem) -> Symbol)
(define (print-lritems lritems) (for-each (lambda (x) (print-lritem x)) lritems))

(define (print-dfa dfa-items) (for-each (lambda (x) (printf "From:~a, Input:~a, Action:~a, Reaction:~a~n" (dfa-item-state x) (dfa-item-input x) (dfa-item-action x) (dfa-item-reaction x))) dfa-items))

;==============================================================================================
;==== Testing
;==============================================================================================

(define start-rule (rule 'S (list 'Ep '$)))
(define terminals (list 'a 'b))
(define non-terminals (list 'S 'E 'A 'B))
(define rules 
   (list start-rule
    (rule 'Ep (list 'Ep 'E))
    (rule 'E (list 'A '+ 'a))
    (rule 'E (list 'A '+ 'b))
    (rule 'A (list 'a))))
    ;(rule 'B (list 'b))))

;(print-lritems (create-lr-state (list startrule) rules empty))
(print-dfa (parse-grammar (lritem 0 start-rule) (gensym) terminals non-terminals rules))
;(printf "~n")
(create-lr-state (list (lritem 1 (rule 'Ep (list 'Ep 'E)))) rules empty)