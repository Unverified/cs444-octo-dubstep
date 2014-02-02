#lang racket
(require racket/set)

(provide print-machine)
(provide m-only-epsilon)
(provide m-single-char)
(provide concat)
(provide union)
(provide kleene-star)
(provide copy-machine)
(provide nfa->dfa)

(provide m-add-new-start)
(provide m-add-epsilon-transitions)

(provide print-machine)
(provide opt)
(provide process-char)
(provide is-state-accepting)
(provide machine-start)
(provide get-m-md-As)
(provide machine-md)
(provide m-only-epsilon-md)
(provide get-m-state-trans)
(provide get-trans-char)
(provide epsilon)
(provide m-add-transitons)
(provide (struct-out transition))


(define epsilon #\ε)
;(struct: transition ([from : Symbol] [char : Char] [to : Symbol]))
(struct transition (from char to) #:transparent)
;(struct: machine ([states : (Listof Symbol)] [start : Symbol] [accepting : (Listof Symbol)] [transitions : (Listof transition)] [md : (Listof (Pair Symbol A)]))
(struct machine (states start accepting transitions md) #:transparent)

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

;[md : (Listof (Pair Symbol A)]
;This functions is complicated because md could have different levels of lists based on if the machine is
;an nfa or dfa. Don't mess with this function unless you know exactly what your doing
(define [get-m-md-As m state func]
  (define pairs (filter (lambda (pair) (equal? state (first pair))) (machine-md m)))		;gets all (Pair Symbol A) where Symbol == state
  (define all-md-As (append-map (lambda (pair) (first (rest pair))) pairs))			;strips out the A from all the (Pair Symbol A) in pairs
  (define func-filter (filter (lambda (A) (if (list? A) (func (first A)) (func A))) all-md-As))	;filters all As by the passed in function
  (map (lambda (A) (if (list? A) (first A) A)) func-filter))					;Checks if the As are packaged in a list and strips them out

;(: is-state-accepting : machine Symbol -> Boolean)
;checks if state is an accepting state in machine m
(define [is-state-accepting m state]
  (member state (machine-accepting m)))

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

(define [m-add-transitons m transitions]
  (machine (machine-states m)
           (machine-start m)
           (machine-accepting m)
           (append transitions (machine-transitions m))
           (machine-md m)))

;(: m-add-new-start : machine Symbol Symbol -> machine)
;creates a new machine with a new starting state and a transition from the new starting state to
;starting state of m
(define [m-add-new-start m sym md-A]
  (define new-start (gensym))
  (machine (cons new-start (machine-states m)) 
           new-start 
           (machine-accepting m) 
           (cons (transition new-start sym (machine-start m)) (machine-transitions m))
           (cons (list new-start (list md-A)) (machine-md m))))

;(: m-add-epsilon-transitions : machine machine -> machine)
;creates a new machine with epsilon transtion from the start state of m1 to the start state m2
(define [m-add-epsilon-transition m1 m2]
  (machine (append (machine-states m1) (machine-states m2))
           (machine-start m1)
           (append (machine-accepting m1) (machine-accepting m2))
           (cons (transition (machine-start m1) epsilon (machine-start m2)) (append (machine-transitions m1) (machine-transitions m2)))
           (append (machine-md m1) (machine-md m2))))

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
                   (append-map machine-transitions machines))
           (apply append (map machine-md machines))))

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
  (machine new-states 
           (machine-start m1) 
           (machine-accepting m2) 
           (append added-transitions (machine-transitions m1) (machine-transitions m2))
           (append (machine-md m1) (machine-md m2))))

;(: kleene-star : machine -> machine)
;add the kleene-star property to a machine (complete 0.. times)
(define [kleene-star m]
  (define new-start (gensym))
  (define add-new-start (curry cons new-start))
  (define add-rec-transitions (curry append (map (lambda (x) (transition x epsilon new-start)) (machine-accepting m))))
    (machine (add-new-start (machine-states m))
             new-start
             (list new-start)
             (cons (transition new-start epsilon (machine-start m)) (add-rec-transitions (machine-transitions m)))
             (machine-md m)))

;(: kleene-plus : machine -> machine)
;add the kleene-cross property to a machine (complete 1.. times)
(define [kleene-cross m]
  (define rec-transitions (map (lambda (x) (transition x epsilon (machine-start m))) (machine-accepting m)))
  (machine (machine-states m)
           (machine-start m)
           (machine-accepting m)
           (append (machine-transitions m) rec-transitions)
           (machine-md m)))

;(: nfa->dfa : machine -> machine )
(define (nfa->dfa m)
  (define alph (get-m-alphabet m))
  ;(: contains-state-set? machine (Setof Symbol) -> Boolean)
  ;same as contains-state? but works for the sets of states we use here.
  (define (contains-state-set? m state-set)
	(ormap (curry equal? state-set) (machine-states m)))

  ;(: process-states : machine (Listof Symbol) Char -> (Setof Symbol) )
  ;generates all states reachable from the given states with input ch.
  (define (process-states m states ch)
    (list->set (foldr append empty (for/list ([st states]) (process-char m st ch)))))

  ;(: new-dfa-trans : machine (Listof Symbol) -> (Listof transition) )
  ;;generates the transitions from the set of states for all characters in the machine's alphabet
  (define (new-dfa-trans m states)
    ;;trans-set: (Listof (Listof Symbol) Char (Setof Symbol))
    ;;contains all the transitions for the set
    (define trans-set (map (lambda (x) (list (list->set states) x (process-states m states x))) alph))
    (filter-not (lambda (x) (set-empty? (transition-to x))) (map (curry apply transition) trans-set)))
  ;(: compose-dfa : (Listof (Setof Symbol)) machine -> machine )
  ;accumulative recursion
  (define (compose-dfa dfa-states m-out)
    (cond [(empty? dfa-states) m-out]
          [(contains-state-set? m-out (first dfa-states)) (compose-dfa (rest dfa-states) m-out)]
          [else (let* ([new-trans (new-dfa-trans m (set->list (first dfa-states)))]
                       [next-states (filter-not (curry contains-state-set? m-out) (map transition-to new-trans))]
                       [nfa-md (get-md-list m (set->list (first dfa-states)))]
                       [newdfa-md (cond [(empty? nfa-md) empty]
                                        [else (list (list (first dfa-states) nfa-md))])])
                  (compose-dfa (append (rest dfa-states) next-states) 
                               (machine (cons (first dfa-states) (machine-states m-out))
                                        (machine-start m-out)
                                        (if (ormap (lambda (x) (ormap (curry equal? x) (machine-accepting m))) (set->list (first dfa-states))) (cons (first dfa-states) (machine-accepting m-out))
												 (machine-accepting m-out))
                                        (append new-trans (machine-transitions m-out))
                                        (append newdfa-md (machine-md m-out)))))]))

  (let ([new-start (list->set (cond [(equal? (machine-start m) 'rev-start) (rest (e-closure m (machine-start m)))]
                                    [else (e-closure m (machine-start m))]))])
    (compose-dfa (list new-start) (machine empty new-start empty empty empty))))

;(: get-md-list : machine (Listof Symbol) -> (Listof A) )
(define (get-md-list m states)
  (map second (filter list? (map (curry get-md m) states))))

;(: get-md : machine Symbol -> A)
(define (get-md m s)
  (assoc s (machine-md m)))

;(: add-md : machine Symbol A -> machine)
(define (add-md m state data)
  (machine (machine-states m)
           (machine-start  m)
           (machine-accepting m)
           (machine-transitions m)
           (cond [(and (contains-state? m state) (false? (assoc (machine-md m))))
                  (cons (list state data) (machine-md m))]
                 [(false? (assoc (machine-md m))) (error "metadata already exists for state ~a" state)]
                 [else (error "can't add metadata for state that doesnt exist")])))


(define (reverse-m m)
  (cond [(> (length (machine-accepting m)) 1) 
         (let ([new-start 'rev-start])
           (machine
            (cons new-start (machine-states m))
            new-start
            (list (machine-start m))
            (append (map (curry transition new-start epsilon) (machine-accepting m)) (map reverse-t (machine-transitions m)))
            (machine-md m)))]
        [(machine
          (machine-states m)
          (first (machine-accepting m))
          (list (machine-start m))
          (map reverse-t (machine-transitions m))
          (machine-md m))]))


(define (reverse-t t)
  (transition (transition-to t) (transition-char t) (transition-from t)))
;==============================================================================================
;==== Creation
;==============================================================================================

;(: m-only-epsilon : void -> machine)
(define [m-only-epsilon]
  (define start (gensym))
  (machine (list start) start (list start) empty empty)) 

;(: m-only-epsilon : void -> machine)
(define [m-only-epsilon-md md-A]
  (define start (gensym))
  (machine (list start) start (list start) empty (list (list start (list md-A))))) 

;(: m-single-char : Char -> machine)
;creates a machine that recognises a single character
(define [m-single-char ch]
  (define start (gensym))
  (define char  (gensym))
  (machine (list start char) start (list char) (list (transition start ch char)) empty))

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
           (map (lambda (x) (transition (translate (transition-from x)) (transition-char x) (translate (transition-to x)))) (machine-transitions m))
           (map (lambda (x) (list (translate (first x)) (second x))) (machine-md m))))

(define (opt m)
  (nfa->dfa (reverse-m (nfa->dfa (reverse-m m)))))

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
                             (transition 1state2 #\t 1state3))
                       empty))
(define test2 (machine (list 2start 2state1)
                       2start
                       (list 2state1)
                       (list (transition 2start epsilon 2state1)
                             (transition 2state1 #\a 2start))
                       empty))
                       
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
                               (transition 'D #\0 'C))
                         empty))

(printf "~n")
(printf "~n~n~nDFA of ClassEx:~n")
(print-machine (nfa->dfa classex))


;a NFA that was defined in class in the second lecture
(define classex-2 (machine '(A B C)
                         'A
                         '(C)
                         (list
                          (transition 'A #\0 'B)
                          (transition 'A #\1 'A)
                          (transition 'B #\0 'B)
                          (transition 'B #\1 'C))
                         empty))

(print-machine classex-2)
(printf "~n")
(printf "~n~n~nDFA of ClassEx-2:~n")
(print-machine (nfa->dfa classex-2))
(print-machine (opt classex-2))


(define ntest (union (list (m-single-char #\a) (m-single-char #\b))))
(opt ntest)

;(machine-md (copy-machine (nfa->dfa classex-2)))
