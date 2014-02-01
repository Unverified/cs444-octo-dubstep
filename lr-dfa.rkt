#lang racket

(require "machine.rkt")

(provide lr-dfa-start-state)
(provide lr-dfa-reduce)
(provide lr-dfa-shift)
(provide print-rule)
(provide rule)
(provide rule?)
(provide rule-lhs)
(provide rule-rhs)

(define NULL 'null)

;==============================================================================================
;==== Structs
;==============================================================================================

;(struct: rule ([lhs : Symbol] [rhs : (Listof Symbol)]))
(struct rule (lhs rhs))

;(struct: item ([dot : Integer] [rule : (Listof rule)]))
(struct lritem (dot rule))

(struct reduce (rule follow-set))

;==============================================================================================
;==== Struct Functions
;==============================================================================================

(define (get-dot-sym item)
  (define dot (lritem-dot item))
  (define rhs (rule-rhs (lritem-rule item)))
  (if (>= dot (length rhs)) 'epsilon (list-ref rhs dot)))

(define (inc-dot item)
  (lritem (+ 1 (lritem-dot item)) (lritem-rule item)))

(define (rule-eq? r1 r2)
  (cond
    [(or (not (rule? r1)) (not (rule? r2))) #f]
    [else (and (equal? (rule-lhs r1) (rule-lhs r2)) (equal? (rule-rhs r1) (rule-rhs r2)))]))
  

;==============================================================================================
;==== Print Functions
;==============================================================================================

;(: print-rule : rule -> Symbol)
(define (print-rule rule) (printf "~a -> ~a~n" (rule-lhs rule) (rule-rhs rule)))
(define (print-rules rules)
  (for-each (lambda (rule) (print-rule rule)) rules))

;(: print-lritem : lritem -> Symbol)
(define (print-lritem lritem) (printf "~a : " (lritem-dot lritem)) (print-rule (lritem-rule lritem)))

;(: print-lritems : (Listof lritem) -> Symbol)
(define (print-lritems lritems) (for-each (lambda (x) (print-lritem x)) lritems))

(define (print-reduce reduce)  (print-rule (reduce-rule reduce)) (printf " : ~a~n" (reduce-follow-set reduce)))

(define (print-reduces reduces) (for-each (lambda (x) (print-reduce x)) reduces))

;==============================================================================================
;==== Functions
;==============================================================================================

(define (is-terminal sym terminals)
  (list? (member sym terminals)))

(define (is-non-terminal sym non-terminals)
  (list? (member sym non-terminals)))

;(: get-rules : Symbol (Listof rule) -> (Listof rule)
; returns all the rules that have non-terminal lhs as the lhs of the rule
(define (get-rules lhs rules)
  (cond
    [(empty? rules) empty]
    [(equal? lhs (rule-lhs (first rules))) (cons (first rules) (get-rules lhs (rest rules)))]
    [else (get-rules lhs (rest rules))]))

;(: get-rules-omit-list : Symbol (Listof rule) (Listof rule) -> (Listof rule)
; returns all the rules that have non-terminal lhs as the lhs of the rule and omits all rules in 
; omit-rules
(define (get-rules-omit-list lhs rules omit-rules)
  (remove* omit-rules (get-rules lhs rules) rule-eq?))

;(: get-rules-omit-list : Symbol (Listof rule) Symbol -> (Listof rule)
; returns all the rules that have non-terminal lhs as the lhs of the rule and omits the omit-rule
; rule.
(define (get-rules-omit lhs rules omit-rule)
  (get-rules-omit-list lhs rules (list omit-rule)))

;==============================================================================================
;==== Nullable
;==============================================================================================

;(: nullable-rule : rule (Listof Symbol) (Listof Symbol) (Listof rule) -> Boolean
;Checks if all symbols in the rhs of rule all nullable
(define (nullable-rule rule terminals non-terminals rules)
  (cond
    [(empty? (rule-rhs rule)) #t]
    [else (andmap (lambda (sym) (nullable-helper sym rule terminals non-terminals rules)) (rule-rhs rule))]))

;(: nullable-helper : Symbol rule (Listof Symbol) (Listof Symbol) (Listof rule) -> Boolean
;Checks if a symbol is nullable by getting all rules with lhs == sym and not eqaul to parent-rule, then checks
;the rhs of all those rules.              
(define (nullable-helper sym parent-rule terminals non-terminals rules)
  (cond
    [(is-terminal sym terminals) #f]
    [(is-non-terminal sym non-terminals) 
     (ormap 
      (lambda (rule) (nullable-rule rule terminals non-terminals rules)) 
      (get-rules-omit sym rules parent-rule))]))

(define (nullable sym terminals non-terminals rules)
  (nullable-helper sym NULL terminals non-terminals rules))

;==============================================================================================
;==== First
;==============================================================================================

;(: lr-first-handle-loop : (Listof Symbol) rule (Listof Symbol) (Listof Symbol) (Listof rule) -> (Listof Symbol)
;In lr-first-list, if a symbol in the rhs equals the lhs of a rule then there is a loop in the grammar. This method
;will check if the symbol is nullable, if it is then we need to check only the rest of the symbols in the rhs, else
;we stop checking symbols in the rhs. 
(define (lr-first-handle-loop syms rule terminals non-terminals rules)
  (cond
    [(nullable (first syms) terminals non-terminals rules) (lr-first-list (rest syms) rule terminals non-terminals rules)]
    [else empty]))

;(: lr-first-list (Listof Symbol) rule (Listof Symbol) (Listof Symbol) (Listof rule) -> (Listof Symbol)
;Gets the first set of a list of symbols (the rhs of a rule)
(define (lr-first-list syms rule terminals non-terminals rules)
  (cond
    [(empty? syms) empty]
    [(is-terminal (first syms) terminals) (list (first syms))]
    [(equal? (first syms) (rule-lhs rule)) (lr-first-handle-loop syms rule terminals non-terminals rules)]
    [(nullable (first syms) terminals non-terminals rules) 
     (append (lr-first-helper (first syms) rule terminals non-terminals rules) 
             (lr-first-list (rest syms) rule terminals non-terminals rules))]
    [else (lr-first-helper (first syms) rule terminals non-terminals rules)]))

;(: lr-first-helper : Symbol rule (Listof Symbol) (Listof Symbol) (Listof rule) -> (Listof Symbol)
;Gets the first set of a non-terminal symbol sym.
(define (lr-first-helper sym parent-rule terminals non-terminals rules)
  (cond
    [(is-terminal sym terminals) (list sym)]
    [else (append-map 
           (lambda (rule) (lr-first-list (rule-rhs rule) rule terminals non-terminals rules)) 
           (get-rules-omit sym rules parent-rule))]))

;(: lr-first : Symbol (Listof Symbol) (Listof Symbol) (Listof rule) -> (Listof Symbol)
;Call this
(define (lr-first sym terminals non-terminals rules)
  (lr-first-helper sym NULL terminals non-terminals rules))

;==============================================================================================
;==== Follow
;==============================================================================================

(define (follow item follow-local terminals non-terminals rules)
  (define follow-sym (get-dot-sym (inc-dot item)))
  (cond
    [(equal? follow-sym 'epsilon) follow-local]
    [else (lr-first follow-sym terminals non-terminals rules)])) 

;==============================================================================================
;==== NFA
;==============================================================================================

;( lr-nfa : lritem (Listof Symbol) (Listof Symbol) (Listof rule) -> machine
;Recursively builds a machine that represents an lr nfa. First if grabs all the rules with the lhs
;eqaul to the symbol that is infront of the dot in "item". It then calls lr-nfa using those new rules.
;Then (in the cond) it checks if dot-sym is null, ie the dot is at the ent of the rule, and returns an
;epsilon only machine.
;If the dot-sym is not null then we increment the dot in item and call lr-nfa with it to get a machine.
;We then add an epsilon transition from that machine to all the machines in the list new-machines.
(define (lr-nfa item follow-local terminals non-terminals rules)
  (define dot-sym (get-dot-sym item))
  (define new-rules (if (is-non-terminal dot-sym non-terminals) (get-rules dot-sym rules) empty))
  (define new-follow (follow item follow-local terminals non-terminals rules))
  (define new-machines (map (lambda (new-rule) (lr-nfa (lritem 0 new-rule) new-follow terminals non-terminals rules)) new-rules))
  (cond
    [(equal? dot-sym 'epsilon) (printf "reduce rule: ") (print-rule (lritem-rule item)) (printf "~a~n" follow-local) (m-only-epsilon-md (reduce (lritem-rule item) follow-local))]
    [else (m-add-epsilon-transitions (m-add-new-start (lr-nfa (inc-dot item) follow-local terminals non-terminals rules) dot-sym) new-machines)]))

;==============================================================================================
;==== Shift/Reduce
;==============================================================================================

(define (lr-dfa-shift state sym)
  (define new-state (process-char lr-dfa state sym))
  (cond
    [(empty? new-state) #f]
    [else (first new-state)]))

(define (lr-dfa-reduce-helper reduces next-sym)
  (define rule-to-reduce (memf (lambda (reduce) (list? (member next-sym (reduce-follow-set reduce)))) reduces))
  (cond
    [(list? rule-to-reduce) (reduce-rule (first rule-to-reduce))]
    [else #f]))

(define (lr-dfa-reduce state next-sym)
  (cond
    [(is-state-accepting lr-dfa state) (lr-dfa-reduce-helper (get-m-md-As lr-dfa state) next-sym)]
    [else #f]))    

;==============================================================================================
;==== Creation
;==============================================================================================

(define start-rule (rule 'Sp (list 'S)))
(define terminals (list 'a '=))
(define non-terminals (list 'Sp 'S 'E))
(define rules 
  (list 
   start-rule
   (rule 'S (list 'a))
   (rule 'S (list 'E '= 'E))
   (rule 'E (list 'a))))

(define lr-dfa (nfa->dfa (lr-nfa (lritem 0 start-rule) (list 'EOF) terminals non-terminals rules)))

(define lr-dfa-start-state (machine-start lr-dfa))
(printf "~n====== LR DFA ======~n")
(print-machine lr-dfa)


;==============================================================================================
;==== Testing
;==============================================================================================

;(lr-first 'A terminals non-terminals rules)

;(lr-nfa (lritem 0 start-rule) (list NULL) terminals non-terminals rules)
