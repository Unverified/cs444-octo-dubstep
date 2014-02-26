#lang racket

(require "parse-tree.rkt")
(require "scanner.rkt")

(provide weeder)

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
  (parse-tree ast-tree 'UNARY_MINUS look-for-literal andmap))

;==============================================================================================
;==== Check valid void types
;==============================================================================================

(define (check-valid-type tree)
  (define valid-void-type-nodes (list 'NORMAL_METHOD_DECLARATION 'NORMAL_METHOD_DECLARATION_NO_BODY 'FINAL_METHOD_DECLARATION 'STATIC_METHOD_DECLARATION 'ABSTRACT_METHOD_DECLARATION))
  (define is-void (tree? (find-tree 'void (find-child-tree 'TYPE (tree-child-trees tree)))))
  (or (not is-void) (and is-void (member (node-sym (tree-node tree)) valid-void-type-nodes))))

(define (check-void-types ast-tree)
  (define all-trees-containing-type (find-all-trees-containing-child 'TYPE ast-tree))
  (andmap (lambda (tree) (check-valid-type tree)) all-trees-containing-type))

;==============================================================================================
;==== Check constructor names
;==============================================================================================

(define (check-constructor-names ast-tree filename)
  (define all-constructor-id-nodes (find-node-list 'id (find-all-trees-list 'METHOD_DECLARATOR (find-all-trees 'CONSTRUCTOR_DECLARATION ast-tree))))
  (andmap (lambda(id-node) (equal? filename (token-lexeme (leafnode-token id-node)))) all-constructor-id-nodes))

;==============================================================================================
;==== Check class names
;==============================================================================================

(define (check-class-name ast-tree filename)
  (define class-node (find-tree 'CLASS_OR_INTERFACE_DECLARATION ast-tree))
  (define id-leafnode (tree-node (list-ref (tree-child-trees (first (tree-child-trees class-node))) 3)))
  (equal? filename (token-lexeme (leafnode-token id-leafnode))))

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
  (define all-cast-nodes (find-all-trees 'CAST ast-tree))
  (define expected-symss (list (list 'EXPRESSION 'ASSIGNMENT_EXPRESSION 'LOGICAL_OR 'LOGICAL_AND 'BITWISE_OR 'BITWISE_AND 'EQUALITY 'RELATIONAL 'ADDITIVE 'MULTIPLICATIVE 'UNARY_MINUS 'UNARY_NOT 'POSTFIX_EXPRESSION 'IDS)
                               (list 'PRIMITIVE_TYPE)
                               (list 'ARRAY_TYPE)))
  (andmap (lambda (cast-tree) (ormap (lambda (expected-syms) (list? (check-tree-structure (list (first (rest (tree-child-trees cast-tree)))) expected-syms))) expected-symss)) all-cast-nodes))

(define (check-has-constructor ast-tree)
  (define class-tree (find-tree 'CLASS_DECLARATION ast-tree))
  (if (tree? class-tree) (tree? (find-tree 'CONSTRUCTOR_DECLARATION ast-tree)) #t))
  
;==============================================================================================
;==== Execution
;==============================================================================================

(define (weeder filename ast-tree)
  (and (check-class-name ast-tree filename)
       (check-cast-expressions ast-tree)
       (check-has-constructor ast-tree)
       (check-constructor-names ast-tree filename)
       (check-void-types ast-tree)
       (check-decimal-bounds ast-tree)))

