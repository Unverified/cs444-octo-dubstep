#lang racket
(require "scanner.rkt")

(provide (struct-out tree))
(provide (struct-out node))
(provide (struct-out leafnode))

(provide find-node)
(provide find-tree)
(provide print-tree)
(provide is-node-equal)
(provide find-node-list)
(provide find-tree-list)
(provide find-all-nodes)
(provide find-all-trees)
(provide find-child-tree)
(provide find-all-nodes-list)
(provide find-all-trees-list)
(provide find-all-trees-containing-child)

;(struct tree ([sym : Symbol] [child-trees : (Listof tree)]))
(struct tree (node child-trees) #:transparent)
(struct node (sym) #:transparent)
(struct leafnode (token) #:transparent)

(define (find-all-trees sym AST)
  (define (recurse child-trees) (append-map (lambda (tree) (find-all-trees sym tree)) child-trees))
  (cond
    [(and (leafnode? (tree-node AST)) (equal? sym (token-type (leafnode-token (tree-node AST))))) (cons AST (recurse (tree-child-trees AST)))]
    [(and (node? (tree-node AST)) (equal? sym (node-sym (tree-node AST)))) (cons AST (recurse (tree-child-trees AST)))]
    [else (recurse (tree-child-trees AST))]))

(define (find-tree sym AST)
  (cond
    [(and (leafnode? (tree-node AST)) (equal? sym (token-type (leafnode-token (tree-node AST))))) AST]
    [(and (node? (tree-node AST)) (equal? sym (node-sym (tree-node AST)))) AST]
    [else (findf (lambda (tree) (tree? (find-tree sym tree))) (tree-child-trees AST))]))

(define (find-all-nodes sym AST)
  (map (tree-node) (find-all-trees sym AST)))

(define (find-node sym AST)
  (define tree (find-tree sym AST))
  (cond
    [(tree? tree) (tree-node tree)]
    [else #f]))

(define (find-all-trees-list sym ASTs)
  (append-map (lambda (AST) (find-all-trees sym AST)) ASTs))

(define (find-all-nodes-list sym ASTs)
  (append-map (lambda (AST) (find-all-nodes sym AST)) ASTs))

(define (find-tree-list sym ASTs)
  (map (lambda (AST) (find-tree sym AST)) ASTs))

(define (find-node-list sym ASTs)
  (map (lambda (AST) (find-node sym AST)) ASTs))

(define (is-node-equal sym AST)
  (cond
    [(and (leafnode? (tree-node AST)) (equal? sym (token-type (leafnode-token (tree-node AST))))) #t]
    [(and (node? (tree-node AST)) (equal? sym (node-sym (tree-node AST)))) #t]
    [else #f]))

(define (find-child-tree sym ASTs)
  (findf (lambda (AST) (is-node-equal sym AST))  ASTs))

(define (tree-contians-child sym ASTs)
  (tree? (find-child-tree sym ASTs)))

;Finds all the trees that have sym as one of its child nodes
(define (find-all-trees-containing-child sym AST)
  (define (recurse child-trees) (append-map (lambda (tree) (find-all-trees-containing-child sym tree)) child-trees))
  (cond
    [(tree-contians-child sym (tree-child-trees AST)) (cons AST (recurse (tree-child-trees AST)))]
    [else (recurse (tree-child-trees AST))]))

;==============================================================================================
;==== Print Functions
;==============================================================================================

(define (print-tree tree)
  (cond
    [#t
      (define (print-tree tree indentation)
        (define treenode (tree-node tree))
        (cond
          [(leafnode? treenode) (printf "~aleafnode | " indentation) (print-token (leafnode-token treenode))]
          [else (printf "~anode | ~a~n" indentation (node-sym treenode))])        
        (for-each (lambda (child-node) (print-tree child-node (string-append "  " indentation))) (tree-child-trees tree)))
      (print-tree tree "")]
    [else (printf "")]))