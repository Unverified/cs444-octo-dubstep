#lang racket

(require "ast-tree.rkt")
(require "enviroments.rkt")
(require "type-linker.rkt")

(provide check-heirarchy)

(define universal-base-class '("java" "lang" "Object"))

;(define typelink-lists '())

(define (fully-qualified-name-equal? fqn uqn)
  '())

(define (fqn->uqn name)
  (cond
    [(empty? name) (printf "~a is empty!" name) (error "OYT")]
    [else  (list (last name))]))

(define (uqn? name)
  (empty? (rest name)))

(define fqn?
  (lambda (name) (not (uqn? name))))

(define (fqn-qualifier fqn)
  (reverse (rest (reverse fqn))))



(define (type-equal? t1 t2 typelink-list typelink-lists)
  
  '(match (list t1 t2)
     [(list (ptype _ typ1) (ptype _ typ2)) (symbol=? typ1 typ2)]
     [(list (atype _ typ1) (atype _ typ2)) (type-equal? typ1 typ2 typelink-list typelink-lists)]
     [(list (rtype _ l1) (rtype _ l2)) (equal? (link-full (get-any-link l1 typelink-list)) (link-full (get-any-link l2 typelink-list)))]
     [(list _ _) #f]))


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


(define (get-all-parents name env)
  (let ([ast (get-class-ast name env)])
    (match ast
      [(interface _ _ _ _ extends _) extends]
      [(class _ _ _ _ extends implements _) (append extends implements)])))

(define (get-extends-interface-names name env)
  (let ([ast (get-class-ast name env)])
    (match ast
      [(interface _ _ _ _ extends  _) extends])))


(define (get-implements-class-names name env)
  (let ([ast (get-class-ast name env)])
    (match ast
      [(class _ _ _ _ _ implements _) implements])))

    
(define (get-extends-class-name name env)
  (let ([ast (get-class-ast name env)])
    (match ast
      [(class _ _ _ _ extends _ _) extends]
      [_ empty])))

(define (get-base-class-names name env)
  (let ([ast (get-class-ast name env)])
    (match ast
      [(interface _ _ _ _ extends _) extends]
      [(class _ _ _ _ extends implements _) (append (list implements) extends)])))



;(define (get-inherited-interface-methods link typelink-lists)
;  (let ([interface-names 
  
        



(define (get-all-fully-qualified-names L)
  (define (recurse env-list)
    (cond
      [(empty? env-list) empty]
      [else (cons (link-full (second (first env-list))) (recurse (rest env-list)))]))
      
  (cond
    [(empty? L) empty]
   ; [(false? (second (first (first env-list)))) 
    [else (append  (recurse (first L)) (get-all-fully-qualified-names (rest L)))]))


(define (get-any-link name current-typelink-list typelink-lists)
  (cond
    [(uqn? name) (get-link name current-typelink-list)]
    [else (get-fqn-link name typelink-lists)]))

;;Gets the link for a fully-qualified name
(define (get-fqn-link fqn typelink-lists)
  (let 
      ([link (get-link (fqn->uqn fqn) (first typelink-lists))])
    (if link (if (equal? (link-full link) fqn) link (get-fqn-link fqn (rest typelink-lists))) 
        (get-fqn-link fqn (rest typelink-lists)))))

(define (get-link uqn typelink-list)
  (let ([ret (assoc uqn typelink-list)])
    (if 
     ret
     (second (assoc uqn typelink-list))
     #f)))

(define (get-all-links L)
  (foldr (lambda (x y) (append (foldr (lambda (x y) (cons (second x) y)) empty x) y)) empty L))


;;gets the file-environment that a fully-qualified name belongs to
;;Useful for preventing name conflicts
(define (get-fqn-typelink-list fqn typelink-lists)
  (printf "~a~n" fqn)
  (let*
      ([typelink-list (first typelink-lists)]
       [link (get-link (fqn->uqn fqn) typelink-list)])
    (if link
        (if (equal? (link-full link) fqn) 
            typelink-list 
            (get-fqn-typelink-list fqn (rest typelink-lists)))
        (get-fqn-typelink-list fqn (rest typelink-lists)))))
                               


(define (augment-environment link typelink-lists previously-visited-classes)


  (cond
    
    [(not (is-class? link)) (printf "ERROR: Class ~a extends interface ~a. Try implements instead~n" (first (fqn->uqn (first previously-visited-classes))) (first (fqn->uqn (link-full link)))) (exit 42)]
    ;[(false? link) (printf "~a~n" previously-visited-classes) (error "You dun goofed")]
    [(member (link-full link) previously-visited-classes) (printf "~a~n" "Circularity in inheritance heirarchy!") (exit 42)]
    [(equal? (link-full link) universal-base-class)  (link-env link)]
    [else      
     (let* ([parent-class (get-extends-class-name (first (fqn->uqn (link-full link))) (link-env link))]
            ;[nothing (printf "~a~n" parent-class)]
            [parent-class-fqn
             (cond
               [(empty? parent-class) empty]
               [(uqn? parent-class) (append (fqn-qualifier (link-full link)) parent-class)]
               [else parent-class])]
            ;[nothing-2 (printf "~a~n" parent-class-fqn)]
            [env (cond
                  [(empty? parent-class-fqn) (augment-environment (get-fqn-link universal-base-class typelink-lists) '()  (cons (link-full link) previously-visited-classes))]
                  ;[(uqn? parent-class) (augment-environment (get-fqn-link (append (fqn-qualifier (link-full link)) parent-class) typelink-lists) typelink-lists (cons (link-full link) previously-visited-classes))]
                  [else (augment-environment (get-fqn-link parent-class-fqn typelink-lists) typelink-lists (cons (link-full link) previously-visited-classes))])])
       (merge-environments (link-env link) env '() typelink-lists))]))
           
                  



;;augment-class-env : envs (listof (String ((listof String) env)))


;;merge environments : envs envs -> envs
(define (merge-environments derived-env base-env current-typelink-list typelink-lists)

  ;;get-type : funt or eval -> type
  (define (get-type f env)
    (assoc f (envs-types env)))
  
  (define (check-single-method method derived-methods)
    (cond 
      [(pair? (assoc (first method) derived-methods))
       (printf "~a~n"  (second (get-type (first method) base-env)))
       (printf "~a~n" (second (get-type (first method) derived-env)))
       (let* ([type-1 (second (get-type (first method) base-env))]
              [type-2 (second (get-type (first method) derived-env))])
         (cond
           [(type-equal? type-1 type-2 current-typelink-list typelink-lists) empty]
           [else (printf "Inherited method ~a has different return type from parent~n"
                           method)
                   (exit 42)]))]
      [else empty]))
           
      
  
  (define (check-single-field field derived-fields)
    (cond
      [(pair? (assoc (first field) derived-fields))
       (let* ([val (second (assoc (first field) (envs-vars derived-env)))]
              [scope-1 (var-scope (eval-ast val))]
              [scope-2 (var-scope (eval-ast (second (assoc (first field) (envs-vars base-env)))))])
         (cond [(equal? scope-1 scope-2) empty]
               [else (printf "Field ~a has different permission from its parent~n" (first field))
                     (exit 42)]))]
      [else empty]))
             
        
  
  (define (check-each-method)
    (map (lambda (x) (check-single-method x (envs-methods derived-env))) (envs-methods base-env)))
 
  
  (define (check-each-field)
    (map (lambda (x) (check-single-field x (envs-vars derived-env))) (envs-vars base-env)))
  


  
  
  
  (check-each-method)
  (check-each-field)
  
  (env-append derived-env base-env))
   
         
(define (is-class? link)
  (let ([ast (get-class-ast (first (fqn->uqn (link-full link))) (link-env link))])
    (match ast
      [(class _ _ _ _ _ _ _) #t]
      [_ #f])))
         
(define (process-link link typelink-lists)
  ;(get-link (fqn->uqn universal-base-class) (get-fqn-typelink-list universal-base-class typelink-lists)))
  ;(list (link-full link) (if (is-class? link) (get-extends-class-name (first (fqn->uqn (link-full link))) (link-env link)) 'Interface)))
  (if (is-class? link) (augment-environment link typelink-lists empty) 'Interface))

;; typelink-list : List of association lists matching fully qualified names to links
;;these association-lists may be called the "file-environment" for a particular file
;; A Link is pair of (fully-qualified-names, envs) (Also, the Hero of Time)
(define (check-heirarchy typelink-lists)
  (map (lambda (link) (process-link link typelink-lists)) (get-all-links typelink-lists)))
  ;(get-all-links typelink-lists))
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
