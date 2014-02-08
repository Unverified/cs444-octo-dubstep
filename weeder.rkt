#lang racket

(require "ast-tree.rkt")
(require "scanner.rkt")

(provide weeder)
(provide weeder-set-debug-mode)

;==============================================================================================
;==== Debug
;==============================================================================================

(define debug-mode #f)

(define (weeder-set-debug-mode mode)
  (set! debug-mode mode))

;==============================================================================================
;==== Weeder Checks
;==============================================================================================

(define (check-valid-type tree)
  (define valid-void-type-nodes (list 'DECL_NO_BODY 'NORMAL_FUNC 'FINAL_FUNC 'STATIC_FUNC 'ABS_FUNC))
  (define is-void (tree? (find-tree 'void (find-child-tree 'TYPE (tree-child-trees tree)))))
  (or (not is-void) (and is-void (member (node-sym (tree-node tree)) valid-void-type-nodes))))

(define (check-void-types ast-tree)
  (if (debug-mode) (printf "Checking void only in function types.~n") (printf ""))
  (define all-trees-containing-type (find-all-trees-containing-child 'TYPE ast-tree))

  (define result (andmap (lambda (tree) (check-valid-type tree)) all-trees-containing-type))

  (if (debug-mode) (printf "Result: ~a~n" result) (printf ""))
  result)

(define (check-constructor-names ast-tree filename)
  (if (debug-mode) (printf "Checking constructor names equal filename: \"~a\"~n" filename) (printf ""))
  (define all-constructor-id-nodes (find-node-list 'id (find-all-trees-list 'FUNC_BODY (find-all-trees 'CONSTRUCTOR ast-tree))))
  (define result (andmap (lambda(id-node) (equal? filename (token-lexeme (leafnode-token id-node)))) all-constructor-id-nodes))
  (if (debug-mode) (printf "Result: ~a~n" result) (printf ""))
  result)

(define (check-class-name ast-tree filename)
  (if (debug-mode) (printf "Checking class/interface name equals filename: \"~a\"~n" filename) (printf ""))
  (define jclass-node (find-tree 'JCLASS ast-tree))
  (define id-leafnode (tree-node (list-ref (tree-child-trees (first (tree-child-trees jclass-node))) 3)))
  (define result (equal? filename (token-lexeme (leafnode-token id-leafnode))))
  (if (debug-mode) (printf "Result: ~a~n" result) (printf ""))
  result)

(define (check-cast-structure ast-tree expected-syms)
  (cond
    [(empty? expected-syms) #t]
    [(empty? ast-tree) #f]
    [(> (length ast-tree) 1) #f]
    [(and (node? (tree-node (first ast-tree))) (equal? (first expected-syms) (node-sym (tree-node (first ast-tree))))) 
;     (printf "cast: ~a : ~a~n" (tree-node (first ast-tree)) (first expected-syms))
     (check-cast-structure (tree-child-trees (first ast-tree)) (rest expected-syms))]
    [(and (leafnode? (tree-node (first ast-tree))) (equal? (first expected-syms) (token-type (leafnode-token (tree-node (first ast-tree)))))) 
 ;    (printf "cast: ~a : ~a~n" (tree-node (first ast-tree)) (first expected-syms))
     (check-cast-structure (tree-child-trees (first ast-tree)) (rest expected-syms))]
    [else #f]))

(define (check-cast-expressions ast-tree)
  (if (debug-mode) (printf "Checking Cast Nodes for correct form.~n") (printf ""))
  (define all-cast-nodes (find-all-trees 'CAST ast-tree))
 ; (printf "CASES: ~a~n" all-cast-nodes)
  (define expected-symss (list (list 'COND_ARITH_EXP 'CP6 'CP5 'CP4 'CP3 'CP2 'CP2b 'CP1 'CP0 'ARITH_EXP 'ARITH_TERM 'ARITH_FACTOR 'IDS)
                              (list 'PRIMITIVE)))
;  (andmap (lambda (cast-tree) (check-cast-structure (list (first (rest (tree-child-trees cast-tree)))) (first expected-symss))) all-cast-nodes)
  (define result (andmap (lambda (cast-tree) (ormap (lambda (expected-syms)(check-cast-structure (list (first (rest (tree-child-trees cast-tree)))) expected-syms)) expected-symss)) all-cast-nodes))
  (if (debug-mode) (printf "Result: ~a~n" result) (printf ""))
  result)

(define (check-has-constructor ast-tree)
  (if (debug-mode) (printf "Checking existance of constructor.~n") (printf ""))
  (define result (tree? (find-tree 'CONSTRUCTOR ast-tree)))
  (if (debug-mode) (printf "Result: ~a~n" result) (printf ""))
  result)
  

;==============================================================================================
;==== Execution
;==============================================================================================

(define (weeder-run filename ast-tree)
  (cond
    [(not (check-class-name ast-tree filename)) #f]
    [(not (check-cast-expressions ast-tree)) #f]
    [(not (check-has-constructor ast-tree)) #f]
    [(not (check-constructor-names ast-tree filename)) #f]
    [(not (check-void-types ast-tree)) #f]
    [else #t]))

(define (weeder filename AST)
  (if (debug-mode) (printf "~n========== RUNNING WEEDER ==========~n") (printf ""))
  (weeder-run filename AST))
  
