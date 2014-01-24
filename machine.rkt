#lang racket

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

;(: get-m-trans : (transition -> Boolean) machine -> (Listof transition) )
(define [get-trans F m]
  (filter F (machine-transitions m)))

;(: get-m-state-trans : machine state -> (Listof transition) )
(define [get-m-state-trans m s]
  (get-trans (lambda(x) (equal? (transition-from x) s)) m))

;(: get-trans-char : (Listof transition) char -> (Listof transition) )
(define [get-trans-char t c]
  (filter (lambda (x) (equal? (transition-char x) c)) t))


;==============================================================================================
;==== Machine Processing
;==============================================================================================

;(: process-char : machine Symbol Char -> (Listof Symbol))
(define [process-char m state char]
  (append (append-map (lambda (x) (process-char m x char)) (e-closure m state))
          (map transition-to (get-trans-char (get-m-state-trans m state) char))))

;(: e-closure : machine Symbol -> (Listof Symbol))
(define [e-closure m state]
  (cons state (map transition-to (get-trans-char (get-m-state-trans m state) epsilon))))

;==============================================================================================
;==== Transformations
;==============================================================================================

;(: union : (Listof machine) -> machine)
;generates a machine that is the union of 2 machines
(define [union machines]
  (define new-start (gensym))
  (machine (cons new-start (append-map machine-states machines))
           new-start
           (append-map machine-accepting machines)
           (append (map (lambda (x) (transition new-start epsilon (machine-start x))) machines) 
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
  (define add-rec-transitions (lambda (y) (append y (map (lambda (x) (transition x epsilon new-start)) (machine-accepting m)))))
  (define add-new-start (lambda (x) (cons new-start x)))
    (machine (add-new-start (machine-states m)) 
             new-start 
             (list new-start) 
             (cons (transition new-start epsilon (machine-start m)) (add-rec-transitions (machine-transitions m)))))

;(: kleene-plus : machine -> machine)
;add the kleene-plus property to a machine (complete 1.. times)
(define [kleene-plus m]
  (define rec-transitions (map (lambda (x) (transition x epsilon (machine-start m))) (machine-accepting m)))
  (machine (machine-states m) (machine-start m) (machine-accepting m) (append (machine-transitions m) rec-transitions)))

;(:nfa->dfa  : machine -> machine )


;==============================================================================================
;==== Base Cases
;==============================================================================================

;(: m-only-epsilon : -> machine)
(define [m-only-epsilon]
  (define start (gensym))
  (machine (list start) start (list start) empty)) 

;(: m-single-char : Char -> machine)
;creates a machine that recognises a single character
(define [m-single-char ch]
  (define start (gensym))
  (define char  (gensym))
  (machine (list start char) start (list char) (list (transition start ch char))))

;==============================================================================================
;==== Creation
;==============================================================================================

;(: string->machine : String -> machine)
;create a machine that accepts a string of characters
(define [string-machine str]
  (concat (map m-single-char (string->list str))))

;==============================================================================================
;==== Printing
;==============================================================================================
;(: print-machine : machine -> Symbol)
(define [print-machine m]
  (print-states m)
  (printf "~n")
  (print-transitions m)
  'ok)

;(: print-states : machine -> Symbol)
(define [print-states m]
  (for-each (lambda (x) (if (equal? x (machine-start m)) (printf "~a* " x) (printf "~a " x))) (machine-states m))
  'ok)

;(: print-transitions : machine -> Symbol)
(define [print-transitions m]
  (for-each (lambda (x) (printf "~a(~a) -> ~a~n" (transition-from x) (transition-char x) (transition-to x))) (machine-transitions m))
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

(printf "Test1:~n")
(print-machine test1)
(printf "Test2:~n")
(print-machine test2)
(printf "Test3:~n")
(print-machine test3)
(printf "Test4:~n")
(print-machine test4)
(printf "~n")

(e-closure test2 (machine-start test2))
(process-char test1 (machine-start test1) #\c)
(process-char test3 (machine-start test3) #\c)
(process-char test2 (machine-start test2) #\a)