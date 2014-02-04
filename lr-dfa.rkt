#lang racket

(require "machine.rkt")
(require "token.rkt")

(provide start-rule)
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

(define (loop-follow-helper sym rhs terminals non-terminals rules)
  (cond
    [(<= (length rhs) 1) empty]
    [(equal? sym (first rhs)) (append (lr-first (first (rest rhs)) terminals non-terminals rules) (loop-follow-helper sym (rest rhs) terminals non-terminals rules))]
    [else (loop-follow-helper sym (rest rhs) terminals non-terminals rules)]))

(define (loop-follow sym sym-rules terminals non-terminals rules)
  (append-map (lambda (rule) (loop-follow-helper sym (rule-rhs rule) terminals non-terminals rules)) sym-rules))

(define (follow item follow-local terminals non-terminals rules)
  (define sym (get-dot-sym item))
  (define follow-sym (get-dot-sym (inc-dot item)))
  (define loops-follow (loop-follow sym (get-rules sym rules) terminals non-terminals rules))
  (cond
    [(equal? follow-sym 'epsilon) (append loops-follow follow-local)]
    [else (append loops-follow (lr-first follow-sym terminals non-terminals rules))])) 

;==============================================================================================
;==== NFA
;==============================================================================================
    
(define (m-complete-loops m)
  (define (get-new-epsilon-trans m start state)
    (define state-rules (get-m-md-As m state rule?))
    (define state-trans (get-m-state-trans m state))
    (define e-trans (get-trans-char state-trans epsilon))
    (define non-e-trans (filter (lambda (t) (not (equal? epsilon (transition-char t)))) state-trans))
    (define new-trans (append (append-map (lambda (t) (get-new-epsilon-trans m (transition-to t) (transition-to t))) e-trans)
                              (append-map (lambda (t) (get-new-epsilon-trans m start (transition-to t))) non-e-trans)))
    (define loop (memf (lambda (t) (list? (memf (lambda (r) (equal? (transition-char t) (rule-lhs r))) state-rules))) non-e-trans))
    (cond
      [(and (list? loop) (not (equal? start (transition-from (first loop))))) (cons (transition (transition-from (first loop)) epsilon start) new-trans)]
      [else new-trans]))

  (define new-m-trans (get-new-epsilon-trans m (machine-start m) (machine-start m)))
  (m-add-transitons m new-m-trans))

;( lr-nfa : lritem (Listof Symbol) (Listof Symbol) (Listof rule) -> machine
;Recursively builds a machine that represents an lr nfa. First if grabs all the rules with the lhs
;eqaul to the symbol that is infront of the dot in "item". It then calls lr-nfa using those new rules.
;Then (in the cond) it checks if dot-sym is null, ie the dot is at the ent of the rule, and returns an
;epsilon only machine.
;If the dot-sym is not null then we increment the dot in item and call lr-nfa with it to get a machine.
;We then add an epsilon transition from that machine to all the machines in the list new-machines.
(define (lr-nfa start terminals non-terminals rules)
  (define (gen-machine item follow-local terminals non-terminals rules)
    (define dot-sym (get-dot-sym item))
    (define new-rules (if (is-non-terminal dot-sym non-terminals) (get-rules-omit dot-sym rules (lritem-rule item)) empty))
    (define new-follow (follow item follow-local terminals non-terminals rules))
    (define new-nfas (map (lambda (new-rule) (gen-machine (lritem 0 new-rule) new-follow terminals non-terminals rules)) new-rules))
  ;  (define new-dfas (map (lambda (nfa) (opt (m-complete-loops nfa))) new-nfas))
    (cond
      [(equal? dot-sym 'epsilon) (m-only-epsilon-md (reduce (lritem-rule item) follow-local))]
      [else  (m-add-epsilon-transitions (m-add-new-start (gen-machine (inc-dot item) follow-local terminals non-terminals rules) dot-sym (lritem-rule item)) new-nfas)]))
  (gen-machine (lritem 0 start) (list 'EOF) terminals non-terminals rules))


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
    [(is-state-accepting lr-dfa state) (lr-dfa-reduce-helper (get-m-md-As lr-dfa state reduce?) next-sym)]
    [else #f]))    

;==============================================================================================
;==== Sanity Check 
;==============================================================================================

(define (check-rhs-rule rhs terminals non-terminals)
  (cond
    [(empty? rhs) empty]
    [(and (boolean? (member (first rhs) terminals)) 
          (boolean? (member (first rhs) non-terminals))) 
     (cons (first rhs) (check-rhs-rule (rest rhs) terminals non-terminals))]
    [else (check-rhs-rule (rest rhs) terminals non-terminals)]))
 
(define (check-rules rules terminals non-terminals)
  (cond
    [(empty? rules) empty]
    [else (append (check-rhs-rule (rule-rhs (first rules)) terminals non-terminals)
          (check-rules (rest rules) terminals non-terminals))]))

(define (get-lr-nfa start-rule terminals non-terminals rules)
  (define check (check-rules rules terminals non-terminals))
  (cond
    [(not (empty? check))
     (printf "LR-DFA CREATION ERROR!!~nThe follwing symbols where found in rules but where not present in terminals or non-terminals:~n~a~n" check)
     (exit)]
    [else (lr-nfa start-rule terminals non-terminals rules)]))

;==============================================================================================
;==== Creation
;==============================================================================================

(define start-rule (rule 'S (list 'JCLASS)))
(define terminals (cons 'id (map first token-exps)))
(define non-terminals (list 'S 'JCLASS 'MOD 'DECLS 'DECL 'FUNC 'VAR 'TYPE 'SCOPE 'ARGS 'ARG_LIST 'ARG 'ASSIGN 'LITERAL 'OBJECT 'CMOD 'VMOD 'FMOD 'CONSTRUCTOR))

(define literal-rules
  (list 
    (rule 'LITERAL (list 'id))
    (rule 'LITERAL (list 'null-lit))
    (rule 'LITERAL (list 'bool-lit))
    (rule 'LITERAL (list 'decimal-lit))
    (rule 'LITERAL (list 'octal-lit))
    (rule 'LITERAL (list 'floating-point-lit))
    (rule 'LITERAL (list 'hex-lit))))

(define type-rules
  (list
    (rule 'TYPE (list 'boolean))
    (rule 'TYPE (list 'int))
    (rule 'TYPE (list 'char))
    (rule 'TYPE (list 'byte))
    (rule 'TYPE (list 'short))
    (rule 'TYPE (list 'OBJECT))
    (rule 'OBJECT (list 'id 'dot 'OBJECT))
    (rule 'OBJECT (list 'id))))

(define class-mod-rules
  (list
    (rule 'CMOD (list 'final))
    (rule 'CMOD (list 'static))
    (rule 'CMOD (list 'abstract))
    (rule 'CMOD empty)))

(define func-mod-rules
  (list
    (rule 'FMOD (list 'final))
    (rule 'FMOD (list 'static))
    (rule 'FMOD empty)))

(define var-mod-rules
  (list
    (rule 'VMOD (list 'static))
    (rule 'VMOD empty)))

(define mod-rules (append class-mod-rules func-mod-rules var-mod-rules))

(define scope-rules
  (list
    (rule 'SCOPE (list 'public))
    (rule 'SCOPE (list 'protected))))

(define other-rules 
   (list 
    start-rule
    (rule 'JCLASS (list 'public 'CMOD 'class 'id 'ocurl 'DECLS 'ccurl))
    (rule 'DECLS (list 'DECLS 'DECL))
    (rule 'DECLS (list 'DECL))
    (rule 'DECLS empty)
 ;   (rule 'DECL (list 'CONSTRUCTOR))
    (rule 'DECL (list 'FUNC))
    (rule 'DECL (list 'VAR))
  ;  (rule 'CONSTRUCTOR (list 'SCOPE 'id 'oparen 'ARGS 'cparen 'ocurl 'ccurl))
    (rule 'FUNC (list 'SCOPE 'abstract 'TYPE 'id 'oparen 'ARGS 'cparen 'semi))
    (rule 'FUNC (list 'SCOPE 'FMOD 'TYPE 'id 'oparen 'ARGS 'cparen 'ocurl 'ccurl))
  ;  (rule 'ARGS (list 'ARG_LIST))
    (rule 'ARGS empty)
 ;   (rule 'ARG_LIST (list 'ARG 'comma 'ARG_LIST))
;    (rule 'ARG_LIST (list 'ARG))
;    (rule 'ARG (list 'TYPE 'id))
    (rule 'VAR (list 'SCOPE 'VMOD 'TYPE 'ASSIGN 'semi))
    (rule 'ASSIGN (list 'id 'eq 'LITERAL)) 
    (rule 'ASSIGN (list 'id ))))

(define rules (append other-rules literal-rules type-rules mod-rules scope-rules))

(printf "getting nfa~n")
(define nfa (get-lr-nfa start-rule terminals non-terminals rules))

(printf "Got nfa~n")
(define lr-dfa (nfa->dfa nfa))


(define lr-dfa-start-state (machine-start lr-dfa))

(printf "~n====== LR DFA ======~n")
(print-machine lr-dfa)


;==============================================================================================
;==== Testing
;==============================================================================================

;(lr-first 'A terminals non-terminals rules)

;(lr-nfa (lritem 0 start-rule) (list NULL) terminals non-terminals rules)
