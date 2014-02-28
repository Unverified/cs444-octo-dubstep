#lang racket

(require "ast-tree.rkt")

;;heirarchy :( [id : String] (methods : listof String)) (fields : (listof String)) (derived-classes : (listof heirarchy)) (final? : Boolean ) [native? : Boolean] [static? : Boolean]) 
(struct heirarchy (id methods fields derived-classes scope final? native? static?))



;;build-heirarchy : ast -> heirarchy 

;;cyclic? : (id-getter : Y -> X) (children-getter : Y -> listof Y) (visited-nodes : listof X) (node : Y) -> Boolean
;;Should be able to detect cycle in any graph given

(define (cyclic? id-getter children-getter visited-nodes node)
  (cond
    [(empty? (children-getter node)) #t]
    [(member (id-getter node) visited-nodes) #f]
    [else
     (let [(id (id-getter node))]
       (ormap (lambda (child)
                   (cyclic? id-getter children-getter (cons id visited-nodes) child))
              (children-getter node)))]))
                 


