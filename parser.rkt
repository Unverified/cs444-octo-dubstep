#lang racket

(require racket/include)
(include "tokenizer.rkt")
(include "lr-dfa.rkt")

(struct parser-stack (state token symbol node))

;(struct tree ([sym : Symbol] [child-nodes : (Listof tree)]))
(struct tree (sym child-nodes))

;==============================================================================================
;==== Print Functions
;==============================================================================================

(define (print-token-stack stack)
  (for-each (lambda (token) (printf "~a~n" token)) (parser-stack-token stack)))

(define (print-tree tree)
  (define (print-tree tree indentation)
    (printf "~anode: ~a~n" indentation (tree-sym tree))
    (for-each (lambda (child-node) (print-tree child-node (string-append "  " indentation))) (tree-child-nodes tree)))
  (print-tree tree ""))

;==============================================================================================
;==== Stack Operations
;==============================================================================================

;(: push-token : Symbol parser-stack) -> parser-stack
;push a single token onto the token stack
(define (push-token token stack)
 ; (printf "pushing: ~a~n" token)
  (parser-stack (parser-stack-state stack) (cons token (parser-stack-token stack)) (parser-stack-symbol stack) (parser-stack-node stack)))
(define (push-state state stack) (parser-stack (cons state (parser-stack-state stack)) (parser-stack-token stack) (parser-stack-symbol stack) (parser-stack-node stack)))
(define (push-symbol symbol stack) (parser-stack (parser-stack-state stack) (parser-stack-token stack) (cons symbol (parser-stack-symbol stack)) (parser-stack-node stack)))
(define (push-node tree stack) (parser-stack (parser-stack-state stack) (parser-stack-token stack) (parser-stack-symbol stack) (cons tree (parser-stack-node stack))))

;(: pop-token : parser-stack) -> parser-stack
;pop a single token off the top of the token stack
(define (pop-token stack)
;  (printf "poping: ~a~n" (first (parser-stack-token stack)))
  (parser-stack (parser-stack-state stack) (rest (parser-stack-token stack)) (parser-stack-symbol stack) (parser-stack-node stack)))
(define (pop-state stack) (parser-stack (rest (parser-stack-state stack)) (parser-stack-token stack) (parser-stack-symbol stack) (parser-stack-node stack)))
(define (pop-symbol stack) (parser-stack (parser-stack-state stack) (parser-stack-token stack) (rest (parser-stack-symbol stack)) (parser-stack-node stack)))
(define (pop-node stack) (parser-stack (parser-stack-state stack) (parser-stack-token stack) (parser-stack-symbol stack) (rest (parser-stack-node stack))))

;(: pop-n-token : Integer parser-stack) -> parser-stack
;pop a total of 'amount' tokens off the top of the token stack
(define (pop-n-token amount stack)
  (cond
    [(equal? 0 amount) (parser-stack-token stack)]
    [else (pop-n-token (- amount 1) (pop-token stack))]))

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
;pop a total of n states off the top of the state, token, and node stacks
(define (pop-n n stack)
  (parser-stack (pop-n-state (- n 1) stack) (pop-n-token n stack) (parser-stack-symbol stack) (pop-n-node n stack)))

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
;tries to reduce rules based on whats at the top of the token/state stacks. It calls lr-dfa-reduce
;to ask the lr-dfa if we can reduce using the top of the token stack as input and the top of the state
;stack as the state we are on in the dfa. If we can reduce we pop all the rhs tokens of the token stack
;and we pop rhs tokens - 1 off the state stack and finally push the lhs token on the token stack and recurse.
(define (reduce stack)
  (define rule (lr-dfa-reduce (first (parser-stack-state stack)) (first (parser-stack-token stack))))
  (cond
    [(not (rule? rule)) stack]
    [(and (not (empty? (rest (parser-stack-symbol stack)))) (dfa-lookahead (first (parser-stack-state stack)) (first (parser-stack-token stack)) (token-type (first (rest (parser-stack-symbol stack)))))) stack]
    [(equal? 'epsilon (first (rule-rhs rule)))
     (define top-token (first (parser-stack-token stack)))
     (define lhs (rule-lhs rule))
     (define new-tree (tree (rule-lhs rule) empty))
     (reduce (push-token lhs (push-node new-tree (push-symbol (token top-token "") (pop-token (pop-node stack))))))]
    [else							; no conflict, only reduce rule, so reduce by it
     (define rhs-len (length (rule-rhs rule)))
     (define lhs (rule-lhs rule))
     (define new-tree (tree (rule-lhs rule) (get-n-nodes rhs-len (parser-stack-node stack))))
     (reduce (push-token lhs (push-node new-tree (pop-n rhs-len stack))))]))

;(: parser : (Listof token) parser-stack -> Symbol
;Takes in a list of tokesn and currently returns either the symbol 'OK or 'ERROR. If its 'OK
;then the series of tokens represents a valid joos1w program, if 'ERROR then it was not a valid program.
(define (parser tokens)
  ;(: parser : (Listof token) parser-stack -> parser-stack
  ;recurses through tokens, building a token/state stack as it goes. If it runs out of tokens, or it could
  ;find a transition for the input token on the current lf-dfa state then it returns the current parser-stack
  ;which the parser will check to see it the stack it a valid one
  (define (parse stack)
    (cond
      [(empty? tokens) stack]
      [else
       (define top-sym (token-type (first (parser-stack-symbol stack))))
       (define new-tree (tree top-sym empty))
       (define new-stack (reduce (push-node new-tree (push-token top-sym stack))))
       (define next-state (lr-dfa-shift (first (parser-stack-state new-stack)) (first (parser-stack-token new-stack))))
       (if (not (symbol? next-state)) new-stack (parse (push-state next-state (pop-symbol new-stack))))]))
  
    (define result-stack (parse (parser-stack (list lr-dfa-start-state) empty tokens empty))) ;start the recursive parser function and get a stack back
  
    (printf "DONE PARSING~n")
  
    (printf "Token Stack:~n")
    (print-token-stack result-stack)
  
    (printf "Tree:~n")
    (print-tree (first (parser-stack-node result-stack)))

    ;If the resulting stack has the lhs of the lr-dfas' starting rule on it then we correctly parsed a joos1W program, else fail
    (cond
      [(empty? result-stack) 'ERROR]
      [(equal? (first (parser-stack-token result-stack)) (rule-lhs start-rule)) 'OK]
      [else 'ERROR]))

;==============================================================================================
;==== Execution
;==============================================================================================

(define file "input.txt")
(define clist (string->list (file->string file)))
(define tokens (append (tokenize clist STATE_START "") (list (token '$ "$")))) ;

(printf "~n====== Scanned Tokens ======~n")
(for-each (lambda (x) (printf "~a : ~a~n" (token-type x) (token-lexeme x))) tokens)

(printf "~n====== Parsing Result ======~n")
(parser tokens)
  
