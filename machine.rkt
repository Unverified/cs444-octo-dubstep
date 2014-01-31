#lang racket
(require racket/set)

(provide m-only-epsilon)
(provide m-add-new-start)
(provide m-add-epsilon-transitions)
(provide print-machine)

(define epsilon #\ε)
;(struct: transition ([from : Symbol] [char : Char] [to : Symbol]))
(struct transition (from char to))
;(struct: machine ([states : (Listof Symbol)] [start : Symbol] [transitions : (Listof transition)]))
(struct machine (states start accepting transitions))

;==============================================================================================
;==== Machine Processing
;==============================================================================================

;(: contains-state? : machine Symbol -> Boolean )
(define [contains-state? m s] 
  (ormap (lambda (x) (equal? x s)) (machine-states m)))

;(: get-m-trans : (transition -> Boolean) machine -> (Listof transition))
(define [get-trans F m]
  (filter F (machine-transitions m)))

;(: get-m-state-trans : machine state -> (Listof transition))
(define [get-m-state-trans m s]
  (get-trans (lambda(x) (equal? (transition-from x) s)) m))

;(: get-trans-char : (Listof transition) char -> (Listof transition))
(define [get-trans-char t c]
  (filter (lambda (x) (equal? (transition-char x) c)) t))

;(: get-m-alphabet : machine -> (Listof Char))
;gets the alphabet of the machine, removes epsilon
(define [get-m-alphabet m]
  (rest (remove-duplicates (cons epsilon (map transition-char (machine-transitions m))))))

;==============================================================================================
;==== Machine Processing
;==============================================================================================

;(: process-char : machine Symbol Char -> (Listof Symbol))
(define [process-char m state char]
  (remove-duplicates (append (append-map (lambda (x) (process-char m x char)) (rest (e-closure m state)))
                             (append-map (curry e-closure m) (map transition-to (get-trans-char (get-m-state-trans m state) char))))))

;(: e-closure : machine Symbol -> (Listof Symbol))
(define [e-closure m state]
  (remove-duplicates (cons state (append-map (curry e-closure m) (map transition-to (get-trans-char (get-m-state-trans m state) epsilon))))))

;(: m-transition-table : machine -> (Listof (Listof (Setof Symbol))) )
(define [m-transition-table m]
  (cons (cons 'State (cons epsilon (get-m-alphabet m)))
        (for/list ([st (machine-states m)])
          (cons st
                (cons (list->set (e-closure m st))
                      (for/list ([ch (get-m-alphabet m)])
                        (list->set (process-char m st ch))))))))

;==============================================================================================
;==== Transformations
;==============================================================================================
;(: m-add-new-start : machine Symbol Symbol -> machine)
;creates a new machine with a new starting state and a transition from the new starting state to
;starting state of m
(define [m-add-new-start m sym]
  (define new-start (gensym))
  (machine (cons new-start (machine-states m)) 
           new-start 
           (machine-accepting m) 
           (cons (transition new-start sym (machine-start m)) (machine-transitions m))))

;(: m-add-epsilon-transitions : machine machine -> machine)
;creates a new machine with epsilon transtion from the start state of m1 to the start state m2
(define [m-add-epsilon-transition m1 m2]
  (machine (append (machine-states m1) (machine-states m2))
           (machine-start m1)
           (append (machine-accepting m1) (machine-accepting m2))
           (cons (transition (machine-start m1) epsilon (machine-start m2)) (append (machine-transitions m1) (machine-transitions m2)))))
  

;(: m-add-epsilon-transitions : machine (Listof machine) -> machine)
;creates a new machine with epsilon transtions from the start state of m to the start states ms
(define [m-add-epsilon-transitions m ms]
  (cond
    [(empty? ms) m]
    [else (m-add-epsilon-transitions (m-add-epsilon-transition m (first ms)) (rest ms))]))
  

;(: m-add-state : machine Symbol -> machine)
(define (m-add-state m state)
  (machine (cons state (machine-states m)) (machine-start m) (machine-accepting m) (machine-transitions m)))

;(: union : (Listof machine) -> machine)
;generates a machine that is the union of 2 machines
(define [union machines]
  (define new-start (gensym))
  (define new-end   (gensym))
  (define end-trans (map (lambda (x) (transition x epsilon new-end)) (append-map machine-accepting machines)))
  (machine (cons new-start (cons new-end (append-map machine-states machines)))
           new-start
           (list new-end)
           (append (map (lambda (x) (transition new-start epsilon (machine-start x))) machines)
                   end-trans
                   (append-map machine-transitions machines))))

;(: concat : (Listof machine) -> machine)
(define [concat machines]
  (cond [(empty? machines) (error "can't concatinate empty list of machines")]
        [(equal? (length machines) 1) (first machines)]
        [else (concat-2 (first machines) (concat (rest machines)))]))

;(: concat-2 : machine machine -> machine)
;concats 2 machines together used as base case
(define [concat-2 m1 m2]
  (define new-states (append (machine-states m1) (machine-states m2)))
  (define added-transitions (map (lambda (x) (transition x epsilon (machine-start m2)))(machine-accepting m1)))
  (machine new-states (machine-start m1) (machine-accepting m2) (append added-transitions (machine-transitions m1) (machine-transitions m2))))

;(: kleene-star : machine -> machine)
;add the kleene-star property to a machine (complete 0.. times)
(define [kleene-star m]
  (define new-start (gensym))
  (define add-new-start (curry cons new-start))
  (define add-rec-transitions (curry append (map (lambda (x) (transition x epsilon new-start)) (machine-accepting m))))
    (machine (add-new-start (machine-states m))
             new-start
             (list new-start)
             (cons (transition new-start epsilon (machine-start m)) (add-rec-transitions (machine-transitions m)))))

;(: kleene-plus : machine -> machine)
;add the kleene-cross property to a machine (complete 1.. times)
(define [kleene-cross m]
  (define rec-transitions (map (lambda (x) (transition x epsilon (machine-start m))) (machine-accepting m)))
  (machine (machine-states m) (machine-start m) (machine-accepting m) (append (machine-transitions m) rec-transitions)))

;(: nfa->dfa : machine -> machine )
(define (nfa->dfa m)

  ;(: contains-state? machine (Setof Symbol) -> Boolean)
  ;Lexically-overridden 
  (define (contains-state? m state-set)
	(ormap (curry set=? state-set) (machine-states m)))

  ;(: process-states : machine (Listof Symbol) Char -> (Setof Symbol) )
  ;generates all states reachable from the given states with input ch.
  (define (process-states m states ch)
    (list->set (foldr append empty (for/list ([st states]) (process-char m st ch)))))

  ;(: new-dfa-trans : machine (Listof Symbol) -> (Listof transition) )
  ;;generates the transitions from the set of states for all characters in the machine's alphabet
  (define (new-dfa-trans m states)
    ;;trans-set: (Listof (Listof Symbol) Char (Setof Symbol))
    ;;contains all the transitions for the set
    (define trans-set (map (lambda (x) (list (list->set states) x (process-states m states x))) (get-m-alphabet m)))
    (filter-not (lambda (x) (set-empty? (transition-to x))) (map (curry apply transition) trans-set)))

  ;(: compose-dfa : (Listof (Setof Symbol)) machine -> machine )
  ;accumulative recursion
  (define (compose-dfa dfa-states m-out)
    (cond [(empty? dfa-states) m-out]
          [(contains-state? m-out (first dfa-states)) (compose-dfa (rest dfa-states) m-out)]
          [else (let* ([new-trans (new-dfa-trans m (set->list (first dfa-states)))]
                       [next-states (filter-not (curry contains-state? m-out) (map transition-to new-trans))])
                  (compose-dfa (append (rest dfa-states) next-states) 
                               (machine (cons (first dfa-states) (machine-states m-out))
                                        (machine-start m-out)
                                        (machine-accepting m-out)
                                        (append new-trans (machine-transitions m-out)))))]))

  (let ([new-start (list->set (e-closure m (machine-start m)))])
    (compose-dfa (list new-start) (machine empty new-start empty empty))))

;==============================================================================================
;==== Converting NFAs to DFAs
;==============================================================================================


;;nfa-to-dfa : machine -> machine
;(define [nfa-to-dfa m]
;	(define symbol-list-equal? (lambda (m n)
;					(cond
;						[(empty? m) (empty? n)]
;						[(empty? n) (empty? m)]
;						[else (and (equal? (first m) (first n)) (symbol-list-equal? (rest m) (rest n)))])))
;	;;record-state : (machine  listof(listof(symbol)) listof(listof(symbol)) -> void) machine machine (listof symbol) (listof symbol) -> listof(symbol)
;	(define record-state (lambda (k m d s wl) 
;				((lambda (s)
;					(if (empty? (filter (lambda (x) (symbol-list-equal? x s)) (machine-states d)))
;						(k (machine 
;							(cons s (machine-states d))
;							(machine-start d)
;							(machine-accepting d)
;							(machine-transitions d))
;						   s
;						   (cons s wl))
;						(k d s wl)))
;					(append-map (lambda (x) (e-closure m x)) s))))
;				(record-state (lambda (d s wl) (print-machine (machine (machine-states d) s (machine-accepting d) (machine-transitions d)))) m (machine empty (gensym) empty empty) (list (machine-start m)) empty))
;

;==============================================================================================
;==== Creation
;==============================================================================================

;(: m-only-epsilon : void -> machine)
(define [m-only-epsilon]
  (define start (gensym))
  (machine (list start) start (list start) empty)) 

;(: m-single-char : Char -> machine)
;creates a machine that recognises a single character
(define [m-single-char ch]
  (define start (gensym))
  (define char  (gensym))
  (machine (list start char) start (list char) (list (transition start ch char))))

;(: string->machine : String -> machine)
;create a machine that accepts a string of characters
(define [string-machine str]
  (concat (map m-single-char (string->list str))))

;(: copy-machine : machine -> machine )
(define [copy-machine m]
  (define translations (apply hash (foldr append empty (map (lambda (x) (list x (gensym))) (machine-states m)))))
  (define translate (curry hash-ref translations))
  (machine (map translate (machine-states m))
           (translate (machine-start m))
           (map translate (machine-accepting m))
           (map (lambda (x) (transition (translate (transition-from x)) (transition-char x) (translate (transition-to x)))) (machine-transitions m))))

;==============================================================================================
;==== Printing
;==============================================================================================

;(: print-machine : machine -> Symbol)
(define [print-machine m]
  (print-states m)
  (printf "~n")
  (print-m-transitions m)
  'ok)

;(: print-states : machine -> Symbol)
(define [print-states m]
  (for-each (lambda (x) (cond 
                          [(and (equal? x (machine-start m))
                                (member x (machine-accepting m)))(printf "~a! ~n" x)] ;i failed fizzbuzz
                          [(equal? x (machine-start m))          (printf "~a* ~n" x)]
                          [(member x (machine-accepting m))      (printf "~a? ~n" x)]
                          [else                                  (printf "~a~n" x)])) (machine-states m))
  'ok)

;(: print-m-transitions : machine -> Symbol)
(define [print-m-transitions m]
  (apply print-transition (machine-transitions m))
  'ok)

;(: print-transitions : transition.. -> Symbol )
(define [print-transition . t]
  ;(: print-translation-1 : translation -> Symbol )
  (define [print-transition-1 t]
    (printf "~a(~a) -> ~a~n" (transition-from t) (transition-char t) (transition-to t)))
  (for-each print-transition-1 t)
  'ok)

;==============================================================================================
;==== Testing, remove when done
;==============================================================================================
(define 1start  (gensym))
(define 1state1 (gensym))
(define 1state2 (gensym))
(define 1state3 (gensym))
(define 2start  (gensym))
(define 2state1 (gensym))

(define test1 (machine (list 1start 1state1 1state2 1state3)
                       1start
                       (list 1state3)
                       (list (transition 1start  #\c 1state1)
                             (transition 1state1 #\a 1state2)
                             (transition 1state2 #\t 1state3))))
(define test2 (machine (list 2start 2state1)
                       2start
                       (list 2state1)
                       (list (transition 2start epsilon 2state1)
                             (transition 2state1 #\a 2start))))
                       
(define test3 (union (list test1 test2)))
(define test4 (concat (list test1 test2)))
(define test5 (kleene-star test1))

;a NFA that was defined in class in the second lecture
(define classex (machine '(A B C D)
                         'A
                         '(C D)
                         (list (transition 'A #\0 'B)
                               (transition 'A epsilon 'C)
                               (transition 'B #\1 'B)
                               (transition 'B #\1 'D)
                               (transition 'C epsilon 'B)
                               (transition 'C #\0 'D)
                               (transition 'D #\0 'C))))

(printf "~n")
(printf "~n~n~nDFA of ClassEx:~n")
(print-machine (nfa->dfa classex))
(print-machine (copy-machine (nfa->dfa classex)))
