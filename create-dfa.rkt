#lang racket

(require "regexes.rkt")
(require "token.rkt")
(require "machine.rkt")

(provide all-tokens-machine)

(define dfa-file (string->path "tokens.dfa"))
(define (all-tokens-machine) 
  (if (file-exists? dfa-file)
      (let* ([in (open-input-file dfa-file)]
             [dfa (read in)])
        (close-input-port in)
        (dfa->hashed-dfa dfa))
      (let ([dfa (normalize-m-md (copy-machine (nfa->dfa (union (map (lambda (x) (copy-machine (opt (string->machine (second x) (first x))))) token-exps)))))]
            [out (open-output-file dfa-file)])
        (write dfa out)
        (close-output-port out)
        (dfa->hashed-dfa dfa))))

(define (dfa->hashed-dfa m)
  (machine (machine-states m)
           (machine-start m)
           (machine-accepting m)
           (make-hash (map (lambda(t) (list (get-t-key t) (transition-to t))) (machine-transitions m)))
           (make-hash (machine-md m))))

  
