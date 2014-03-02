#lang racket

(require "ast-tree.rkt")
(require "enviroments.rkt")
(require "type-linker.rkt")

(provide check-heirarchy)

(define universal-base-class "Object")



(define (fully-qualified-name-equal? fqn uqn)
  '())

(define (type-equal? t1 t2 L)
  (match (list t1 t2)
    [(list (ptype typ1) (ptype typ2)) (symbol=? typ1 typ2)]
     [(list (atype typ1) (atype typ2)) (type-equal? typ1 typ2 L)]
     [(list (rtype l1) (rtype l2)) (equal? (link-full (second (assoc l1 L))) (link-full (second (assoc l2 L))))]
     [_ #f]))


(define (check-interfaces current-env-pair L)
  (let* ([id-ast-pair (assoc (first current-env-pair) (envs-types (link-env (second current-env-pair))))]
         [id (first id-ast-pair)]
         [current-ast (second id-ast-pair)]
         ) 
    (current-ast)))




(define (get-class-env name env-assoc-list)
  (link-env (second (assoc name env-assoc-list))))


(define (get-class-ast name env)
  (let* ([types (envs-types env)]
        [ast (second (assoc name types))])
    ast))
    

(define (get-base-class-names name env)
  (let ([ast (get-class-ast name env)])
    (match ast
      [(interface _ _ _ extends _) extends]
      [(class _ _ _ extends implements _) (append (list implements) extends)])))

(define (check-for-cyclic-inheritance ast env-list L)
  (match ast
    [(interface _ _ id extends _) (if (member id L ) #t (andmap (lambda (i) (check-for-cyclic-inheritance (get-class-ast i) env-list (cons id L))) extends))]
    [(class _ _ id extends implements _) (if (member id L) #t (andmap (lambda (i) (check-for-cyclic-inheritance (get-class-ast i) env-list (cons id L))) (append extends implements)))]))



(define (get-all-fully-qualified-names env-list)
  (cond
    [(empty? env-list) empty]
   ; [(false? (second (first (first env-list)))) 
    [else (cons  (first (first env-list)) (get-all-fully-qualified-names (rest env-list)))]))

;;augment-class-env : envs (listof (String ((listof String) env)))

(define (augment-class-env current-class-env-pair L)
  
  ;;check that class implements all functions named in interfaces
  (check-interfaces current-class-env-pair L))
        
         
         


       
(define (check-heirarchy env-list)
  ;(for-each (lambda (x) (if (empty? x) (display "") (display (first x)))) env-list)
  
  (map (lambda (b) (get-base-class-names "Object" b)) (map (lambda (a) (get-class-env '("java" "lang" "Object") a)) env-list)))
  ;(get-all-fully-qualified-names env-list))
  

(struct method-sig (ret-type args))


(define (method-sig-args-equal? m1 m2)
  (andmap (lambda (P) (type-equal? (first P) (second P))) (method-sig-args m1) (method-sig-args m2)))


(define (method-sig-equal? m1 m2)
  (and 
   (type-equal? (method-sig-ret-type m1)
             (method-sig-ret-type m2))
   (method-sig-args-equal? m1 m2)))


       
    
         
;;;cyclic? : (id-getter : Y -> X) (children-getter : Y -> listof Y) (visited-nodes : listof X) (node : Y) -> Boolean
;;;Should be able to detect cycle in any graph given
;;; we'll have to wait and see
;(define (cyclic? id-getter children-getter visited-nodes node)
;  (cond
;    [(member (id-getter node) visited-nodes) #t]
;    [(empty? (children-getter node)) #f]
;    [else
;     (let [(id (id-getter node))
;       
;           (children (children-getter node))]
;       (ormap (lambda (child)
;                   (cyclic? id-getter children-getter (cons id visited-nodes) child))
;              children))]))
;                 
;
;
;
;
;
;  
;
;;;=================================================
;;;testing
;;;=================================================
;(struct simple-graph (id children))
;(cyclic? simple-graph-id simple-graph-children empty (simple-graph 'A (list (simple-graph 'B empty) (simple-graph 'C empty))))
;
;(cyclic? simple-graph-id simple-graph-children empty (simple-graph 'A (list (simple-graph 'B (list (simple-graph 'A empty))) (simple-graph 'C empty))))
;
;
;
