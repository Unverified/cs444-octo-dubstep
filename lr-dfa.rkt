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

;(struct: lritem ([dot : Integer] [rule : (Listof rule)]))
(struct lritem (dot rule))

;(struct: reduce ([rule : rule] [lookahead : Symbol]))
(struct reduce (rule lookahead))

;==============================================================================================
;==== Print Functions
;==============================================================================================

;(: print-rule : rule -> Symbol)
(define (print-rule rule) (printf "~a -> ~a" (rule-lhs rule) (rule-rhs rule)))
(define (print-rules rules)
  (for-each (lambda (rule) (print-rule rule)) rules))

;(: print-lritem : lritem -> Symbol)
(define (print-lritem lritem) (printf "~a : " (lritem-dot lritem)) (print-rule (lritem-rule lritem)))

;(: print-lritems : (Listof lritem) -> Symbol)
(define (print-lritems lritems) (for-each (lambda (x) (print-lritem x)) lritems))

(define (print-reduce reduce)  (print-rule (reduce-rule reduce)) (printf " : ~a~n" (reduce-lookahead reduce)))

(define (print-reduces reduces) (for-each (lambda (x) (print-reduce x)) reduces))

;==============================================================================================
;==== Shift/Reduce
;==============================================================================================

;(: lr-dfa-shift : Symbol Symbol -> Symbol)
(define (lr-dfa-shift state sym)
  (dfa-process-sym lr-dfa state sym))

;(: lr-dfa-reduce-helper : (Listof reduce) Symbol -> [ rule | Boolean ])
(define (lr-dfa-reduce-helper reduces next-sym)
  (define rule-to-reduce (memf (lambda (reduce) (equal? next-sym (reduce-lookahead reduce))) reduces))
  (cond
    [(list? rule-to-reduce) (reduce-rule (first rule-to-reduce))]
    [else #f]))

;(: lr-dfa-reduce : Symbol Symbol -> [ rule | Boolean ])
(define (lr-dfa-reduce state next-sym)
  (cond
    [(is-state-accepting lr-dfa state) (lr-dfa-reduce-helper (get-m-md-As lr-dfa state reduce?) next-sym)]
    [else #f]))    

;==============================================================================================
;==== Parse LR Table Files
;==============================================================================================

;(: string->strings : String -> (Listof Symbol)
;Converts a string to a list of strings
(define (string->strings str)
  (regexp-split #px" " str))

;(: string->symbols : String -> (Listof Symbol)
;Converts a string to a list of symbols
(define (string->symbols str)
  (map string->symbol (string->strings str)))

;(: symbols->rule : (Listof Symbol) -> rule
;Converts a list of symbols to a rule
(define (symbols->rule syms)
  (rule (first syms) (rest syms)))

;(: strip-n-lines : Number (Listof String) -> (Listof String)
;Removes n strings off the top of lines
(define (strip-n-lines n lines)
  (cond
    [(<= n 0) lines]
    [else (strip-n-lines (- n 1) (rest lines))]))

;(: rmv-beginning : (Listof String) -> (Listof String)
;Removes the terminals and non-terminals from the generated table file
(define (rmv-beginning lines)
  (define rmv-terminals (strip-n-lines (+ 1 (string->number (first lines))) lines))
  (define rmv-non-terminals (strip-n-lines (+ 2 (string->number (first rmv-terminals))) rmv-terminals))
  rmv-non-terminals)

;(: get-rules : Number (Listof String) -> (Listof rule)
;Converts n rules from the list of string lines
(define (get-rules n lines)
  (cond
    [(<= n 0) empty]
    [else (cons (symbols->rule (string->symbols (first lines))) (get-rules (- n 1) (rest lines)))]))

;(: m-set-start : Symbol ->  machine
;Creates a machine with only a starting state
(define (m-set-start start)
  (machine (list start) start empty empty empty)) 

;(: m-add-shift : machine Symbol Symbol String (Listof rule) ->  machine
;Adds a transition to the machine m, reation is a string number so we need to append "g" on the front
(define (m-add-shift m from input reaction)
  (define to (string->symbol (string-append "g" reaction)))
  (machine (if (list? (member to (machine-states m))) (machine-states m) (cons to (machine-states m)))
           (machine-start m) 
           (machine-accepting m)
           (cons (transition from input to) (machine-transitions m))
           (machine-md m)))

;(: m-add-reduce : machine Symbol Symbol String (Listof rule) ->  machine
;Adds a md-A to the machine m, state is that state on which to reduce, input is the lookahead, reaction is
;the list-ref for into rules for the rule to reduce by.
(define (m-add-reduce m state input reaction rules)
  (define rule (list-ref rules (string->number reaction)))
  (machine (machine-states m)
           (machine-start m) 
           (if (list? (member state (machine-accepting m))) (machine-accepting m) (cons state (machine-accepting m)))
           (machine-transitions m)
           (cons (list state (list (reduce rule input))) (machine-md m))))

;(: add-to-m : machine String (Listof rule) ->  machine
;Parses the string line, which represents a shift/reduce rull in the lrtable. It then adds it to the machine
(define (add-to-m m line rules)
  (define state (string->symbol (string-append "g" (list-ref line 0))))
  (define input (string->symbol (list-ref line 1)))
  (define action (string->symbol (list-ref line 2)))
  (define reaction (list-ref line 3))
  (cond
    [(equal? action 'shift) (m-add-shift m state input reaction)]
    [else (m-add-reduce m state input reaction rules)]))
           
;(: get-lr-dfa : Number (Listof String) (Listof rule) ->  machine
;Parses all the shift/reduce rules in the lrtable and creates a machine out of them
(define (get-lr-dfa n lines rules)
  (define (get-table-helper m n lines rules)
    (cond
      [(<= n 0) m]
      [else (get-table-helper (add-to-m m (string->strings (first lines)) rules) (- n 1) (rest lines) rules)]))
  (define start-state (string->symbol (string-append "g" (first (string->strings (first lines))))))
  (get-table-helper (m-set-start start-state) n lines rules))

;==============================================================================================
;==== Creation
;==============================================================================================

(define file-lines (rmv-beginning (file->lines "grammar/lrtable")))						;open the file as list of string lines
(define rules (get-rules (string->number (first file-lines)) (strip-n-lines 1 file-lines)))			;get the rules
(define new-file-lines (strip-n-lines (+ 2 (string->number (first file-lines))) file-lines))			;remove the rules from the list of strings
(define lr-dfa (get-lr-dfa (string->number (first new-file-lines)) (strip-n-lines 1 new-file-lines) rules))	;parse the shift/reduce lines, create the dfa

(print-machine lr-dfa)

(define start-rule (first rules))
(define lr-dfa-start-state (machine-start lr-dfa))

;==============================================================================================
;==== Testing
;==============================================================================================

;(lr-first 'A terminals non-terminals rules)

;(lr-nfa (lritem 0 start-rule) (list NULL) terminals non-terminals rules)
