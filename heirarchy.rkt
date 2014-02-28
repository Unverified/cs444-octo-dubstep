#lang racket

(require "ast-tree.rkt")

;;heirarchy :( [id : String] (methods : listof String)) (fields : (listof String)) (derived-classes : (listof heirarchy)) (final? : Boolean ) [native? : Boolean] [static? : Boolean]) 
(struct heirarchy (id methods fields derived-classes scope final? native? static?))



;;build-heirarchy : environment -> heirarchy


;;cyclic? : (id-getter : Y -> X) (children-getter : Y -> listof Y) (visited-nodes : listof X) (node : Y) -> Boolean
;;Should be able to detect cycle in any graph given

(define (cyclic? id-getter children-getter visited-nodes node)
  (cond
    [(member (id-getter node) visited-nodes) #t]
    [(empty? (children-getter node)) #f]
    [else
     (let [(id (id-getter node))
       
           (children (children-getter node))]
       (ormap (lambda (child)
                   (cyclic? id-getter children-getter (cons id visited-nodes) child))
              children))]))
                 
;;testing
(struct simple-graph (id children))
(cyclic? simple-graph-id simple-graph-children empty (simple-graph 'A (list (simple-graph 'B empty) (simple-graph 'C empty))))

(cyclic? simple-graph-id simple-graph-children empty (simple-graph 'A (list (simple-graph 'B (list (simple-graph 'A empty))) (simple-graph 'C empty))))



