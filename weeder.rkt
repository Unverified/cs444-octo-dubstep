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
;==== Check Decimal Bounds
;==============================================================================================

(define (check-decimal-lit-bound number)
  (and (<= number 2147483647) (>= number -2147483648)))

(define (check-unary-minus-op-helper op unary-minus-tree)
  (define expected-syms (list 'UNARY_MINUS 'UNARY_NOT 'POSTFIX_EXPRESSION 'PRIMARY 'PRIMARY_NO_NEW_ARRAY 'LITERAL))
  (define literal-children (check-tree-structure (list unary-minus-tree) expected-syms))
  (define literal-tree (if (list? literal-children) (first literal-children) literal-children))
  (cond
    [(not (tree? literal-tree)) (parse-tree-recursively unary-minus-tree 'UNARY_MINUS look-for-literal andmap)]
    [(not (equal? 'decimal-lit (token-type (leafnode-token (tree-node literal-tree))))) #t]
    [else (check-decimal-lit-bound (op 0 (string->number (token-lexeme (leafnode-token (tree-node literal-tree))))))]))

(define look-for-literal
  (lambda (unary-minus-tree)
    (cond
      [(equal? 'UNARY_MINUS_OP (node-sym (tree-node (first (tree-child-trees unary-minus-tree))))) (check-unary-minus-op-helper - (first (rest (tree-child-trees unary-minus-tree))))]
      [else (check-unary-minus-op-helper + unary-minus-tree)])))

(define (parse-tree-recursively tree sym proc recursive-proc)
  (recursive-proc (lambda (child-tree) (parse-tree child-tree sym proc recursive-proc)) (tree-child-trees tree)))

(define (parse-tree tree sym proc recursive-proc)
  (cond
    [(is-node-equal sym tree) (proc tree)]
    [else (parse-tree-recursively tree sym proc recursive-proc)]))

(define (check-decimal-bounds ast-tree)
  (if (debug-mode) (printf "Checking decimal literal bounds.~n") (printf ""))
  (define result (parse-tree ast-tree 'UNARY_MINUS look-for-literal andmap))
  (if (debug-mode) (printf "Result: ~a~n" result) (printf ""))
  result)

;==============================================================================================
;==== Check valid void types
;==============================================================================================

(define (check-valid-type tree)
  (define valid-void-type-nodes (list 'NORMAL_METHOD_DECLARATION 'NORMAL_METHOD_DECLARATION_NO_BODY 'FINAL_METHOD_DECLARATION 'STATIC_METHOD_DECLARATION 'ABSTRACT_METHOD_DECLARATION))
  (define is-void (tree? (find-tree 'void (find-child-tree 'TYPE (tree-child-trees tree)))))
  (or (not is-void) (and is-void (member (node-sym (tree-node tree)) valid-void-type-nodes))))

(define (check-void-types ast-tree)
  (if (debug-mode) (printf "Checking void only in function types.~n") (printf ""))
  (define all-trees-containing-type (find-all-trees-containing-child 'TYPE ast-tree))
  (define result (andmap (lambda (tree) (check-valid-type tree)) all-trees-containing-type))
  (if (debug-mode) (printf "Result: ~a~n" result) (printf ""))
  result)

;==============================================================================================
;==== Check constructor names
;==============================================================================================

(define (check-constructor-names ast-tree filename)
  (if (debug-mode) (printf "Checking constructor names equal filename: \"~a\"~n" filename) (printf ""))
  (define all-constructor-id-nodes (find-node-list 'id (find-all-trees-list 'METHOD_DECLARATOR (find-all-trees 'CONSTRUCTOR_DECLARATION ast-tree))))
  (define result (andmap (lambda(id-node) (equal? filename (token-lexeme (leafnode-token id-node)))) all-constructor-id-nodes))
  (if (debug-mode) (printf "Result: ~a~n" result) (printf ""))
  result)

;==============================================================================================
;==== Check class names
;==============================================================================================

(define (check-class-name ast-tree filename)
  (if (debug-mode) (printf "Checking class/interface name equals filename: \"~a\"~n" filename) (printf ""))
  (define class-node (find-tree 'CLASS_OR_INTERFACE_DECLARATION ast-tree))
  (define id-leafnode (tree-node (list-ref (tree-child-trees (first (tree-child-trees class-node))) 3)))
  (define result (equal? filename (token-lexeme (leafnode-token id-leafnode))))
  (if (debug-mode) (printf "Result: ~a~n" result) (printf ""))
  result)

(define (check-tree-structure ast-tree expected-syms)
  (cond
    [(empty? expected-syms) ast-tree]
    [(empty? ast-tree) #f]
    [(> (length ast-tree) 1) #f]
    [(and (node? (tree-node (first ast-tree))) (equal? (first expected-syms) (node-sym (tree-node (first ast-tree))))) 
     (check-tree-structure (tree-child-trees (first ast-tree)) (rest expected-syms))]
    [(and (leafnode? (tree-node (first ast-tree))) (equal? (first expected-syms) (token-type (leafnode-token (tree-node (first ast-tree)))))) 
     (check-tree-structure (tree-child-trees (first ast-tree)) (rest expected-syms))]
    [else #f]))

(define (check-cast-expressions ast-tree)
  (if (debug-mode) (printf "Checking Cast Nodes for correct form.~n") (printf ""))
  (define all-cast-nodes (find-all-trees 'CAST ast-tree))
 ; (printf "CASES: ~a~n" all-cast-nodes)
  (define expected-symss (list (list 'EXPRESSION 'ASSIGNMENT_EXPRESSION 'LOGICAL_OR 'LOGICAL_AND 'BITWISE_OR 'BITWISE_AND 'EQUALITY 'RELATIONAL 'ADDITIVE 'MULTIPLICATIVE 'UNARY_MINUS 'UNARY_NOT 'POSTFIX_EXPRESSION 'IDS)
                               (list 'PRIMITIVE_TYPE)
                               (list 'ARRAY_TYPE)))
  (define result (andmap (lambda (cast-tree) (ormap (lambda (expected-syms) (list? (check-tree-structure (list (first (rest (tree-child-trees cast-tree)))) expected-syms))) expected-symss)) all-cast-nodes))
  (if (debug-mode) (printf "Result: ~a~n" result) (printf ""))
  result)

(define (check-has-constructor ast-tree)
  (if (debug-mode) (printf "Checking existance of constructor.~n") (printf ""))
  (define class-tree (find-tree 'CLASS_DECLARATION ast-tree))
  (define result (if (tree? class-tree) (tree? (find-tree 'CONSTRUCTOR_DECLARATION ast-tree)) #t))
  (if (debug-mode) (printf "Result: ~a~n" result) (printf ""))
  result)
  
;==============================================================================================
;==== Execution
;==============================================================================================

(define (weeder-run filename ast-tree)
  (and (check-class-name ast-tree filename)
       (check-cast-expressions ast-tree)
       (check-has-constructor ast-tree)
       (check-constructor-names ast-tree filename)
       (check-void-types ast-tree)
       (check-decimal-bounds ast-tree)))

(define (weeder filename AST)
  (if (debug-mode) (printf "~n========== RUNNING WEEDER ==========~n") (printf ""))
  (weeder-run filename AST))
  
