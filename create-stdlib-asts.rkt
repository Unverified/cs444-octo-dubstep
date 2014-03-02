#lang racket

(provide all-tokens-machine)

(define dfa-file (string->path "tokens.dfa"))
(define (all-tokens-machine) 
  (if (file-exists? dfa-file)
      (let* ([in (open-input-file dfa-file)]
             [dfa (read in)])
        (close-input-port in)
        dfa)
      (let ([dfa (normalize-m-md (copy-machine (nfa->dfa (union (map (lambda (x) (copy-machine (opt (string->machine (second x) (first x))))) token-exps)))))]
            [out (open-output-file dfa-file)])
        (write dfa out)
        (close-output-port out)
        dfa)))
