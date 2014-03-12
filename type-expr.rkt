#lang racket

(require "ast-tree.rkt")
(require "environments.rkt")
(require "heirarchy-checker.rkt")



(provide type-expr)

;;perform-bin-op: type type -> type
(define (perform-bin-op op t1 t2)
  (match (list op t1 t2)
    [(list '+ (rtype _ '(java lang String)) (rtype _ '(java lang String))) (rtype empty '(java lang String))]
    ;;TODO: Verify that binops on two numerics behave like we think they do!
    [(list _ (ptype _ _) (ptype _ _)) (if (and (type-numeric? t1) (type-numeric? t2)) (ptype empty 'int) (error "Attempt to perform binary operation on non-numeric type!"))]
    [_ (error "Undefined Binop!")]))

;;parent-of? rtype rtype envs -> Boolean
(define (parent-of? T S)
  (error "parent-of not implemented"))


;;num-type<? : ptype ptype -> Boolean
(define (num-type<? pt1 pt2)
  (define valid-types '(int short char byte long float double))
  (match (list (ptype-type pt1) (ptype-type pt2))
    [(list 'byte t) (list? (member t '(short int long float double)))]
    [(list 'short t) (list? (member t '(int long float double)))]
    [(list 'char t) (list? (member t '(int long float double)))]
    [(list 'int t) (list? (member t '(long float double)))]
    [(list 'long t) (list? (member t '(float double)))]
    [(list 'float t) (list? (member t '(double)))]
    [_ #f]))


;;class-type? rtype -> Boolean
(define (class-type? r)
  (error "class-type? not implemented"))

;;rtype-can-assign? rtype rtype -> Boolean

(define (rtype-can-assign? T S)
  (cond
    [(class-type? S)  (parent-of? S T)]
    [else (if (class-type? T) 
              (type-ast=? T (rtype empty '(java lang Object)))
              (parent-of? S T))]))
    

;;can-assign? (union ptype rtype atype) (union ptype rtype atype) -> Boolean
(define (can-assign? T S)
  (match (list T S)
    [(list (rtype _ _) (ptype _ 'null)) #t]
    [(list (ptype _ 'boolean) (ptype _ 'boolean)) #t]
    [(list (ptype _ 'boolean) (ptype _ _)) #f]
    [(list (ptype _ _) (ptype _ 'boolean)) #f]
    
    ;;If not Boolean, we know the ptypes are numeric. Check that the conversion is not narrowing
    [(list (ptype _ _) (ptype _ _)) (not (num-type<? T S))]
    [(list (rtype _ _) (ptype _ _)) #f]
    [(list (ptype _ _) (ptype _ 'null)) #f]
    [(list (rtype _ _) (rtype _ _)) (rtype-can-assign? T S)]
    [(list (or
            (rtype _ '(java lang Object))
            (rtype _ '(java io Serializable))
            (rtype _ '(java lang Cloneable)))
           (atype _ _)) #t]
    
    [(list (rtype _ _) (atype _ _)) #f]
    [(list (atype _ (ptype _ _)) (atype _ (ptype _ _))) (type-ast=? (atype-type T) (atype-type S))]
    [(list (atype _ (rtype _ _)) (atype _ (rtype _ _))) (can-assign? (atype-type T) (atype-type S))]
    [(list (atype _ _) (atype _ _)) #f]                                                     
    
    [_ (error "Unimplemented assignment")]))
    

    

;;cast-ptypes : Symbol Symbol -> Boolean
(define (cast-ptypes T S)
  (error "Cast-ptypes not implemented"))


;;castable? (union ptype rtype atype) (union ptype rtype atype) envs -> Boolean
(define (castable? T S env)
  (match (list T S)
    [(list (ptype _ sym1) (ptype _ sym2)) (cast-ptypes sym1 sym2)]
    [(list (atype _ typ1) (atype _ typ2)) (begin (printf "Warning: I'm not sure how to properly cast array types") (castable? typ1 typ2))]
    [(list (rtype _ _) (rtype _ _)) (if (type-ast=? T S) #t (or (can-assign? T S) (can-assign? S T)))]   [(list _ _) (error "Cast type mismatch")]))


;;type-numeric? ptype->Boolean
(define (type-numeric? pt)
  (define valid-types '(int short char byte long float double))
  (match pt
    [(ptype _ typ) (list? (member typ valid-types))]
    [_ #f]))


;;define whole-number? ptype-> Bool
(define (whole-number? pt)
  (define valid-types '(int short char byte))
  (match pt
    [(ptype _ typ) (list? (member typ valid-types))]
    [_ #f]))

;;type-expr : ast -> (union ptype rtype atype)
(define (type-expr ast)
  (define env (ast-envt ast))
  (define (test-specific-bin-op type left right err-string)
    (if (and (type-ast=? type (type-expr left)) (type-ast=? type (type-expr right))) type (error err-string)))

  (define (test-un-op op right)
    (cond
      [(symbol=? op '!) (if (type-ast=? (type-expr right) (ptype empty 'boolean)) (ptype empty 'boolean)
                            (error "! operator expects type boolean"))]
      [(symbol=? op '-) (if (type-numeric? (type-expr right)) (type-expr right)
                            (error "- operator expects numeric type"))]
      [else (error "Unimplemented operator")]))
                        
                      
  (match ast
    [(varuse _ 'this) (error "This not implemented")]
    [(vdecl _ _ _ _ _) (ptype empty 'void)]
    
    [(varassign _ id expr)
     (let ([var-type (type-expr id)])
       (if (can-assign? var-type (type-expr expr))
           var-type
           (error "Type Mismatch in Assignment")))]
    
    [(varuse _ id)
     (match (assoc id (envs-types env))
       [#f (error "Unbound Identifier")]
       [(list a b) b])]
    
    [(literal _ type _) type]
    [(or
      (ptype _ _) (atype _ _ ) (rtype  _ _)) ast]
    
    
    [(cast _ c expr) 
       (if (castable? c (type-expr expr) env) c (error "Invalid Cast"))]
    
    [(iff _ test tru fls) (if (begin  (type-expr tru) (type-expr fls) (type-ast=? test (ptype empty 'boolean))) (ptype empty 'void) (error "Type of Test not Boolean"))]
    
    
    [(while _ test body) (if (begin (type-expr body) (type-ast=? test (ptype empty 'boolean)))
                             (ptype empty 'void)
                             (error "While test not Boolean!"))]
    
    [(for _ init clause update body) (if (begin (type-expr init) (type-expr update) (type-expr body)
                                                (type-ast=? (type-expr clause) (ptype empty 'boolean)))
                                         
                                         (ptype empty 'void)
                                         (error "For test not Boolean!"))]
    
    [(unop _ op right) (test-un-op op right)]
    [(binop _ op left right) (perform-bin-op op (type-expr left) (type-expr right))]
    [(parameter _ type _) type]
    
    
    [(block _ _ statements) (begin (map type-expr statements) (ptype empty 'void))]
    [(arrayaccess _ left index) (if (whole-number? (type-expr index)) 
                                    (if (atype? (type-expr left)) 
                                        (atype-type (type-expr left)) 
                                        (error "Array type expected")) 
                                    (error "Array index expects type int"))]
    [(return _ expr) (type-expr expr)]
    [(arraycreate _ type size) (begin (type-expr type) (if (whole-number? (type-expr size)) 
                                                        (atype type)   
                                                        (error "Array declaration expects numeric type for size")))]
    [(methodcall _ _ _ args) 
     (let* ([method-funt (methodcall->funt ast type-expr)]
            [ret (match (assoc  method-funt (envs-types env))
                      [(list a b) b]
                      [_ (error "No Function of that name")])])
       ret)]
           
    [(methoddecl _ id parameters) (error "Attempt to type Method Declaration")]
    [(method _ _ _ _ _ body) (type-expr body)]
    [(or (class _ _ _ _ _ _ body)
         (interface _ _ _ _ _ body)) (type-expr body)]
    [(cunit _ _ body) (type-expr body)]
    
    [(fieldaccess _ _ field) 
     (match (assoc field (envs-types env))
       [#f (error "Unbound Field Access")]
       [(list a b) b])]
    
    [(classcreate e class params) (error "Classcreate not implemented")]
    [(constructor e scope methoddecl body) (type-expr body)]
    [(keyword e _) (error "keyword not implemented")]
    
    
    
    [_ (error "Type Checker Not Implemented")]))

     
                                                
    