#lang racket

(require "lr-dfa.rkt")
(require "scanner.rkt")
(require "ast-tree.rkt")

(provide parser)
(provide (struct-out parser-stack))
(provide (struct-out tree))
(provide (struct-out node))
(provide (struct-out leafnode))

(struct parser-stack (state node))

;==============================================================================================
;==== Print Functions
;==============================================================================================

(define (print-parser-result result-stack)
  (printf "DONE PARSING~n")
  (printf "Node Stack:~n")
  (for-each (lambda (x) (print-tree x)) (parser-stack-node result-stack)))
  

;==============================================================================================
;==== Stack Operations
;==============================================================================================

(define (push-state state stack) (parser-stack (cons state (parser-stack-state stack)) (parser-stack-node stack)))
(define (push-node tree stack) (parser-stack (parser-stack-state stack) (cons tree (parser-stack-node stack))))

(define (pop-state stack) (parser-stack (rest (parser-stack-state stack)) (parser-stack-node stack)))
(define (pop-node stack) (parser-stack (parser-stack-state stack) (rest (parser-stack-node stack))))

;(: pop-n-state : Integer parser-stack) -> parser-stack
;pop a total of 'amount' states off the top of the state stack
(define (pop-n-state amount stack)
  (cond
    [(equal? 0 amount) (parser-stack-state stack)]
    [else (pop-n-state (- amount 1) (pop-state stack))]))

;(: pop-n-node : Integer parser-stack) -> parser-stack
;pop a total of 'amount' states off the top of the node stack
(define (pop-n-node amount stack)
  (cond
    [(equal? 0 amount) (parser-stack-node stack)]
    [else (pop-n-node (- amount 1) (pop-node stack))]))

;(: pop-n : Integer parser-stack) -> parser-stack
;pop a total of n states off the top of the state, input, and node stacks
(define (pop-n n stack)
  (parser-stack (pop-n-state n stack) (pop-n-node n stack)))

;(: get-n-nodes : Integer parser-stack-node) -> (Listof Symbol)
;gets the first 'amount' Symbols of the node-stack
(define (get-n-nodes amount node-stack)
  (cond
    [(equal? 0 amount) empty]
    [else (cons (first node-stack) (get-n-nodes (- amount 1) (rest node-stack)))]))

;==============================================================================================
;==== Parser
;==============================================================================================

;(: reduce : parser-stack) -> parser-stack
(define (reduce stack next-token)
  (define rule (lr-dfa-reduce (first (parser-stack-state stack)) next-token))
  (cond
    [(not (rule? rule)) stack]
    [else
     (define rhs-len (length (rule-rhs rule)))
     (define lhs (rule-lhs rule))
     (define new-tree (tree (node lhs) (reverse (get-n-nodes rhs-len (parser-stack-node stack)))))
     (define new-stack (push-node new-tree (pop-n rhs-len stack)))
     (define new-state (lr-dfa-shift (first (parser-stack-state new-stack)) lhs))
     (reduce (push-state new-state new-stack) next-token)]))

;(: parser : (Listof token) parser-stack -> Symbol
;Takes in a list of tokens and currently returns either the symbol 'OK or 'ERROR. If its 'OK
;then the series of tokens represents a valid joos1w program, if 'ERROR then it was not a valid program.
(define (parser tokens)
  ;(: parser : (Listof token) parser-stack -> parser-stack
  ;recurses through tokens, building a token/state stack as it goes. If it runs out of tokens, or it could
  ;find a transition for the input token on the current lf-dfa state then it returns the current parser-stack
  ;which the parser will check to see it the stack it a valid one
  (define (parse stack tokens)
    (cond
      [(empty? tokens) stack]
      [else
       (define next-token (token-type (first tokens)))
       (define new-stack (push-node (tree (leafnode (first tokens)) empty) (reduce stack next-token)))
       (define next-state (lr-dfa-shift (first (parser-stack-state new-stack)) next-token))
       (if (not (symbol? next-state)) new-stack (parse (push-state next-state new-stack) (rest tokens)))]))
  
    (define BOF_TOK (token 'BOF "BOF"))
    (define EOF_TOK (token 'EOF "EOF"))
    (define result-stack (parse (parser-stack (list (lr-dfa-shift lr-dfa-start-state (token-type BOF_TOK))) 
                                              (list (tree (leafnode BOF_TOK) empty))) 
                                (append tokens (list EOF_TOK))))
    (reverse (parser-stack-node result-stack))) ;return the node-stack






