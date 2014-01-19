#lang racket

(define epsilon #\ε)

;(struct: state ([id : Symbol] [accepting : Boolean]))
(struct state (id accepting))
;(struct: transition ([from : Symbol] [char : Char] [to : Symbol]))
(struct transition (from char to))
;(struct: machine ([states : (Listof state)] [start : Symbol] [transitions : (Listof transition)]))
(struct machine (states start transitions))

;(: process-char (machine Symbol Char -> (Listof Symbol)))
(define [process-char m state char]
  ;(: get-transitions (transition -> Boolean))
  (define [get-transitions trans] (and (equal? (transition-from trans) state) 
                                       (equal? (transition-char trans) char)))
  (append (remove-duplicates (map (lambda (x) (process-char m x char)) (e-closure m state)))
          (map transition-to (filter get-transitions (machine-transitions m)))))

;(: e-closure (machine Symbol -> (Listof Symbol)))
(define [e-closure m state]
  ;(: get-e-transitions (transition -> Boolean))
  (define [get-e-transitions trans] (and (equal? (transition-from trans) state)
                                         (equal? (transition-to trans) epsilon)))
  (map transition-to (remove-duplicates (filter get-e-transitions (machine-transitions m)))))

;(: union ((Listof machine) -> machine))
(define [union machines]
  (define new-start (state (gensym) #f))
  (machine (cons new-start (append-map machine-states machines))
           (state-id new-start)
           (append (map (lambda (x) (transition (state-id new-start) epsilon (machine-start x))) machines) 
                        (append-map machine-transitions machines))
           ))

;==============================================================================================
;==== Printing
;==============================================================================================
;(: print-machine (machine -> Symbol))
(define [print-machine m]
  (print-states m)
  (print-transitions m)
  'ok)

;(: print-states (machine -> Symbol))
(define [print-states m]
  (for-each (lambda (x) (printf "~a: ~a~n" (state-id x) (state-accepting x))) (machine-states m))
  'ok)

;(: print-transitions (machine -> Symbol))
(define [print-transitions m]
  (for-each (lambda (x) (printf "~a(~a) -> ~a~n" (transition-from x) (transition-char x) (transition-to x))) (machine-transitions m))
  'ok)

;==============================================================================================
;==== Testing
;==============================================================================================
(define start  (state (gensym) #f))
(define state1 (state (gensym) #f))
(define state2 (state (gensym) #f))
(define state3 (state (gensym) #t))

(define test1 (machine (list start state1 state2 state3)
                       (state-id start)
                       (list (transition (state-id start)  #\c (state-id state1))
                             (transition (state-id state1) #\a (state-id state2))
                             (transition (state-id state2) #\t (state-id state3)))))
(define test2 (union (list test1)))

(printf "Test1:~n")
(print-machine test1)
(printf "Test2:~n")
(print-machine test2)
(printf "~n")

(process-char test1 (machine-start test1) #\c)
(process-char test2 (machine-start test1) #\c)
