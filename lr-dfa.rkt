
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
  (if (empty? new-lritems) lritems (append lritems (create-lr-state new-lritems rules added-lhs))))

(define (get-rule item rules)
  (define rule (lritem-rule item))
  (cond
    [(empty? rules) (rule 'RULE_NOT_FOUND empty)]
    [(and (equal? (rule-lhs rule) (rule-lhs (first rules))) (equal? (rule-rhs rule) (rule-rhs (first rules)))) (first rules)]
    [else (get-rule item (rest rules))]))

;(: parse-grammar : lritem (Listof Symbol) (Listof Symbol) (Listof rule) -> (Listof something)
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
        [(can-reduce next-item) (cons (dfa-item state (get-dot-sym x) 'reduce (get-rule x rules)) empty)]
        [else (cons (dfa-item state (get-dot-sym x) 'shift next-state) (parse-grammar next-item next-state terminals non-terminals rules))])))
  (append-map handle-lritem (create-lr-state (list item) rules empty)))

;(: replace-state : Symbol Symbol (Listof dfa-item) -> (Listof dfa-item)
;used to replace all instances of old-state with new-state in dfa-items
(define (replace-state new-state old-state dfa-items)
  (map (lambda (x) (if (equal? old-state (dfa-item-state x)) (dfa-item new-state (dfa-item-input x) (dfa-item-action x) (dfa-item-reaction x)) x)) dfa-items))

;(: merge-state dfa-item (Listof dfa-item)
;looks through dfa-items for any items that match dfa-item state and input, if we find on then we have a state that is branching to
;two different states on the same input so we need to merge those states using replace-state
(define (merge-state dfa-item dfa-items)
  (cond
    [(empty? dfa-items) empty]
    [(and (equal? (dfa-item-state dfa-item) (dfa-item-state (first dfa-items))) (equal? (dfa-item-input dfa-item) (dfa-item-input (first dfa-items)))) (merge-state dfa-item (replace-state (dfa-item-reaction dfa-item) (dfa-item-reaction (first dfa-items)) (rest dfa-items)))]
    [else (append (list (first dfa-items)) (merge-state dfa-item (rest dfa-items)))]))

;(: merge-states (Listof dfa-item)
;goes through dfa-items, merging any states that have shifts on the same input to different states
(define (merge-states dfa-items)
  (cond
    [(equal? 1 (length dfa-items)) dfa-items]
    [else (append (list (first dfa-items)) (merge-states (merge-state (first dfa-items) (rest dfa-items))))]))

;==============================================================================================
;==== Printing
;==============================================================================================

;(: print-rule : rule -> Symbol)
(define (print-rule rule) (printf "~a -> ~a~n" (rule-lhs rule) (rule-rhs rule)))

;(: print-lritem : lritem -> Symbol)
(define (print-lritem lritem) (printf "~a : " (lritem-dot lritem)) (print-rule (lritem-rule lritem)))

;(: print-lritems : (Listof lritem) -> Symbol)
(define (print-lritems lritems) (for-each (lambda (x) (print-lritem x)) lritems))

(define (print-dfa dfa-items) (for-each (lambda (x) 
  (cond
    [(equal? 'shift (dfa-item-action x)) (printf "From:~a, Input:~a, Action:~a, Reaction:~a~n" (dfa-item-state x) (dfa-item-input x) (dfa-item-action x) (dfa-item-reaction x))]    
    [(equal? 'reduce (dfa-item-action x)) (printf "From:~a, Input:~a, Action:~a, Rule: " (dfa-item-state x) (dfa-item-input x) (dfa-item-action x)) (print-rule (dfa-item-reaction x))])) dfa-items))

;==============================================================================================
;==== Creation
;==============================================================================================

(define start-rule (rule 'S (list 'jclass '$)))
(define terminals (list 'PUBLIC 'SEMI_COLON 'CLASS 'FINAL 'ABSTRACT 'ID 'LBRACE 'RBRACE 'LBRACKET 'LPAREN 'RPAREN 'RBRACKET 'PUBLIC 'PROTECTED 'STATIC 'BOOLEAN 'INT 'CHAR 'BYTE 'SHORT 'ASSIGN))
(define non-terminals (list 'S 'jclass 'class 'statements 'statement 'declarations 'declaration 'function 'variable 'scope 'type))
(define rules 
   (list start-rule
    (rule 'jclass (list 'PUBLIC 'modifier 'CLASS 'ID 'LBRACE 'declarations 'RBRACE))
    (rule 'modifier (list 'FINAL))
    (rule 'modifier (list 'STATIC))
    (rule 'modifier (list 'epsilon))
    (rule 'declarations (list 'declarations 'declaration))
    (rule 'declarations (list 'declaration))
    (rule 'declarations (list 'epsilon))
    (rule 'declaration (list 'function))
    (rule 'declaration (list 'variable))
    (rule 'function (list 'scope 'modifier 'type 'ID 'LPAREN 'RPAREN 'LBRACE 'RBRACE))
    (rule 'scope (list 'PUBLIC))
    (rule 'scope (list 'PROTECTED))
    (rule 'type (list 'BOOLEAN))
    (rule 'type (list 'INT))
    (rule 'type (list 'CHAR))
    (rule 'type (list 'BYTE))
    (rule 'type (list 'SHORT))
    (rule 'variable (list 'scope 'modifier 'type 'ID 'assignment 'SEMI_COLON))
    (rule 'assignment (list 'ID 'ASSIGN ))
    (rule 'assignment (list 'epsilon ))))

(define lr-dfa (merge-states (parse-grammar (lritem 0 start-rule) (gensym) terminals non-terminals rules)))
(define lr-dfa-start-state (dfa-item-state (first lr-dfa)))

(printf "====== lr-dfa ======~n")
(print-dfa lr-dfa)

;==============================================================================================
;==== Access
;==============================================================================================

(define (lr-dfa-reduce state input)
  (define item (memf (lambda (dfa-item) (and (equal? state (dfa-item-state dfa-item)) (equal? input (dfa-item-input dfa-item)))) lr-dfa))
  (cond
    [(list? item) (dfa-item-reaction (first item))]	;we found a reduce rule that applies to state on input
    [(equal? 'epsilon input) #f]				;we didn't find a rule so we ran this again to see if state this state had a nullable rule
    [else (lr-dfa-reduce state 'epsilon)]))		;look through the states for a rule that is nullable, there should be only one nullable rule on a state

(define (lr-dfa-shift state input)
  (define item (memf (lambda (dfa-item) (and (equal? state (dfa-item-state dfa-item)) (equal? input (dfa-item-input dfa-item)))) lr-dfa))
    (cond
    [(list? item) (dfa-item-reaction (first item))]
    [else #f]))

;==============================================================================================
;==== Testing
;==============================================================================================
  
