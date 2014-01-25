#lang racket

(require racket/include)
(include "tokenizer.rkt")
(include "lr-dfa.rkt")

(struct parser-stack (state token symbol))

;==============================================================================================
;==== Print Functions
;==============================================================================================

(define (print-token-stack stack)
  (for-each (lambda (token) (printf "~a~n" token)) (parser-stack-token stack)))

;==============================================================================================
;==== Stack Operations
;==============================================================================================

;(: push-token : Symbol parser-stack) -> parser-stack
;push a single token onto the token stack
(define (push-token token stack)
;  (printf "pushing: ~a~n" token)
  (parser-stack (parser-stack-state stack) (cons token (parser-stack-token stack)) (parser-stack-symbol stack)))

;(: push-state : Symbol parser-stack) -> parser-stack
;push a single state onto the state stack
(define (push-state state stack) (parser-stack (cons state (parser-stack-state stack)) (parser-stack-token stack) (parser-stack-symbol stack)))

;(: push-state : Symbol parser-stack) -> parser-stack
;push a single state onto the state stack
(define (push-symbol symbol stack) (parser-stack (parser-stack-state stack) (parser-stack-token stack) (cons symbol (parser-stack-symbol stack))))

;(: pop-token : parser-stack) -> parser-stack
;pop a single token off the top of the token stack
(define (pop-token stack)
;  (printf "poping: ~a~n" (first (parser-stack-token stack)))
  (parser-stack (parser-stack-state stack) (rest (parser-stack-token stack)) (parser-stack-symbol stack)))

;(: pop-state : parser-stack) -> parser-stack
;pop a single state off the top of the state stack
(define (pop-state stack) (parser-stack (rest (parser-stack-state stack)) (parser-stack-token stack) (parser-stack-symbol stack)))

;(: pop-n-token : Integer parser-stack) -> parser-stack
;pop a total of 'amount' tokens off the top of the token stack
(define (pop-n-token amount stack)
  (cond
    [(equal? 0 amount) stack]
    [else (pop-n-token (- amount 1) (pop-token stack))]))

;(: pop-n-state : Integer parser-stack) -> parser-stack
;pop a total of 'amount' states off the top of the state stack
(define (pop-n-state amount stack)
  (cond
    [(equal? 0 amount) stack]
    [else (pop-n-state (- amount 1) (pop-state stack))]))

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
    [(equal? 'epsilon (first (rule-rhs rule)))
     (define top-token (first (parser-stack-token stack)))
     (set! stack (pop-token stack))
     (set! stack (push-symbol top-token stack))
     (set! stack (push-token (rule-lhs rule) stack))
     (reduce stack)]
    [else 
     (set! stack (pop-n-token (length (rule-rhs rule)) stack))
     (set! stack (pop-n-state (- (length (rule-rhs rule)) 1) stack))
     (set! stack (push-token (rule-lhs rule) stack))
     (reduce stack)]))

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
       (set! stack (push-token (token-type (first (parser-stack-symbol stack))) stack))
       (set! stack (reduce stack))
       (define next-state (lr-dfa-shift (first (parser-stack-state stack)) (first (parser-stack-token stack))))
;(printf "next-state: ~a~n" next-state)
       (if (not (symbol? next-state)) stack (parse (parser-stack (cons next-state (parser-stack-state stack)) (parser-stack-token stack) (rest (parser-stack-symbol stack)))))]))
  
    (define result-stack (parse (parser-stack (list lr-dfa-start-state) empty tokens))) ;start the recursive parser function and get a stack back
  
    (printf "DONE PARSING, stack:~n")
    (print-token-stack result-stack)

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
  
