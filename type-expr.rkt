#lang racket

(require "errorf.rkt")
(require "class-info.rkt")
(require "ast-tree.rkt")
(require "environments.rkt")
(require "heirarchy-checker.rkt")

(provide type-check)

;;perform-bin-op: type type -> type
(define (perform-bin-op op t1 t2)
  (match (list op t1 t2)
    
    ;;Special case: Can apply + operator to String/bool, bool/String and String/String:
    [(list '+ (rtype '(java lang String)) (rtype '(java lang String))) (rtype '(java lang String))]
    [(list '+ (rtype '(java lang String)) (ptype 'boolean)) (rtype '(java lang String))]
    [(list '+ (ptype 'boolean) (rtype '(java lang String))) (rtype '(java lang String))]
    
    ;;Can apply == and != to bool/bool:
    [(list (or '== '!=) (ptype 'boolean) (ptype 'boolean)) (ptype 'boolean)]
    
    
    ;;TODO: Verify that binops on two numerics behave like we think they do!
    [(list (or '+ '- '* '/) (ptype _) (ptype _)) (if (and (type-numeric? t1) (type-numeric? t2)) (ptype 'int) (c-errorf "Attempt to perform binary operation on non-numeric type!"))]
    [(list (or '< '> '<= '>= '== '!=) (ptype _) (ptype _))  (and (type-numeric? t1) (type-numeric? t2)) (ptype 'boolean) (c-errorf "Attempt to perform binary operation on non-numeric types!")]
    [_ (c-errorf "Undefined Binop!")]))

;;parent-of? rtype rtype envs -> Boolean
(define (parent-of? T S)
  (error "parent-of not implemented"))


;;num-type<? : ptype ptype -> Boolean
(define (num-type<? pt1 pt2)
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
;;checks to see if source rtype (S) can be assigned to target rtype (T)
(define (rtype-can-assign? T S)
  (cond
    [(class-type? S)  (parent-of? S T)]
    [else (if (class-type? T) 
              (type-ast=? T (rtype '(java lang Object)))
              (parent-of? S T))]))
    

;;can-assign? (union ptype rtype atype) (union ptype rtype atype) -> Boolean
(define (can-assign? T S)
  (match (list T S)
    ;;can assign null to rtype
    [(list (rtype _) (ptype 'null)) #t]
    
    ;;can assign boolean to boolean
    [(list (ptype 'boolean) (ptype 'boolean)) #t]
    
    ;;cannot assign boolean to any other ptype
    [(list (ptype 'boolean) (ptype _)) #f]
    [(list (ptype _) (ptype 'boolean)) #f]
    
    ;;If not Boolean, we know the ptypes are numeric. Check that the conversion is not narrowing
    [(list (ptype _) (ptype _)) (not (num-type<? T S))]
    ;;Cannot assign ptype to an rtype, except null
    [(list (rtype _) (ptype _)) #f]
    ;;cannot assign null to a ptype
    [(list (ptype _) (ptype 'null)) #f]
    
    ;;when assigning rtype to rtype, we must employ the algorithm in $5.2 of the spec
    [(list (rtype _) (rtype _)) (rtype-can-assign? T S)]
    
    ;;we may assign an atype to the following three class/interface types
    [(list (or (rtype '(java lang Object))
               (rtype '(java io Serializable))
               (rtype '(java lang Cloneable)))
           (atype _)) #t]
    
    ;;assigning atypes to any but the three above results in a compile-time error
    [(list (rtype _) (atype _)) #f]
    
    
    ;;assigning an atype of ptypes to an atype of ptypes requires the ptypes be equal
    [(list (atype (ptype _)) (atype (ptype _))) (type-ast=? (atype-type T) (atype-type S))]
    
    ;;assigning an atype of rtypes to an atype of rtypes requires the rtypes to be assignable
    [(list (atype (rtype _)) (atype (rtype _))) (can-assign? (atype-type T) (atype-type S))]
    
    ;;any other assignment involving atypes is a compile-time error
    [(list (atype _) (atype _)) #f]                                                     
    
    [_ (error "Unimplemented assignment")]))
    
;;cast-ptypes : ptype ptype -> Boolean
(define (cast-ptypes T S)
  (match (list T S)
    ;;identity conversions: boolean to boolean
    [(list (ptype 'boolean) (ptype 'boolean)) #t]
    ;;invalid conversions: boolean to anything else
    [(list (ptype 'boolean) _) #f]
    [(list _ (ptype 'boolean)) #f]
    
    ;;remaining conversions: Numeric type to numeric type. Will work out widening/narrowing later.
    [(list (ptype _) (ptype _)) (and (type-numeric? T) (type-numeric? S))]
    
    [_ (c-errorf "Unimplemented ptype cast! ~a ~a" T S)]))
  
;;castable? (union ptype rtype atype) (union ptype rtype atype) envs -> Boolean
(define (castable? T S env)
  (match (list T S)
    [(list (ptype sym1) (ptype sym2)) (cast-ptypes T S)]
    [(list (atype typ1) (atype typ2)) (begin (printf "Warning: I'm not sure how to properly cast array types") (castable? typ1 typ2))]
    [(list (rtype _) (rtype _)) (if (type-ast=? T S) #t (or (can-assign? T S) (can-assign? S T)))]
    [(list _ _) (c-errorf "Cast type mismatch")]))


;;type-numeric? ptype->Boolean
(define (type-numeric? pt)
  (define valid-types '(int short char byte long float double))
  (match pt
    [(ptype typ) (list? (member typ valid-types))]
    [_ #f]))


;;define whole-number? ptype-> Bool
(define (whole-number? pt)
  (define valid-types '(int short char byte))
  (match pt
    [(ptype typ) (list? (member typ valid-types))]
    [_ #f]))

;;type-check : (assoc fullq-names info) -> void
(define (type-check all-cinfo)

;;define get-expr-envs (assoc info) expr -> envs
  (define (get-expr-envs e)
    (match (type-expr e)  
      [(rtype t) (info-env (second (assoc t all-cinfo)))]
      [_ (c-errorf "Expression does not resolve to a class type.")]))

;;type-expr : ast -> (union ptype rtype atype)
  (define (type-expr ast)
    (define env (ast-env ast))
    (define (test-specific-bin-op type left right err-string)
      (if (and (type-ast=? type (type-expr left)) (type-ast=? type (type-expr right))) type (error err-string)))

    (define (test-un-op op right)
      (cond
        [(symbol=? op '!) (if (type-ast=? (type-expr right) (ptype 'boolean)) (ptype 'boolean)
                              (error "! operator expects type boolean"))]
        [(symbol=? op '-) (if (type-numeric? (type-expr right)) (type-expr right)
                              (error "- operator expects numeric type"))]
        [else (error "Unimplemented operator")]))
                        
                      
    (match ast
      [(varuse _ 'this) (error "This not implemented")]
      [(vdecl _ _ _ _ _) (ptype 'void)]
    
      [(varassign _ id expr)
       (let ([var-type (type-expr id)])
         (if (can-assign? var-type (type-expr expr))
             var-type
             (c-errorf "Type Mismatch in Assignment")))]
    
      [(varuse _ id)
       (match (assoc id (envs-types env))
         [#f (c-errorf "Unbound Identifier")]
         [(list a b) b])]
    
      [(literal _ type _) type]
      [(or
        (ptype _) (atype _ ) (rtype  _)) ast]
    
      [(cast _ c expr) 
         (if (castable? c (type-expr expr) env) c (c-errorf "Invalid Cast"))]
    
      [(iff _ test tru fls) (if (begin  (type-expr tru) (type-expr fls) (type-ast=? test (ptype 'boolean))) (ptype 'void) (c-errorf "Type of Test not Boolean"))]
    
    
      [(while _ test body) (if (begin (type-expr body) (type-ast=? test (ptype 'boolean)))
                               (ptype 'void)
                               (c-errorf "While test not Boolean!"))]
    
      [(for _ init clause update body) (if (begin (type-expr init) (type-expr update) (type-expr body)
                                                  (type-ast=? (type-expr clause) (ptype 'boolean)))
                                         
                                           (ptype 'void)
                                           (c-errorf "For test not Boolean!"))]
     
      [(unop _ op right) (test-un-op op right)]
      [(binop _ op left right) (perform-bin-op op (type-expr left) (type-expr right))]
      [(parameter _ type _) type]
    
    
      [(block _ _ statements) (begin (map type-expr statements) (ptype 'void))]
      [(arrayaccess _ left index) (if (whole-number? (type-expr index)) 
                                      (if (atype? (type-expr left)) 
                                          (atype-type (type-expr left)) 
                                          (c-errorf "Array type expected")) 
                                      (c-errorf "Array index expects type int"))]
      [(return _ expr) (type-expr expr)]
      [(arraycreate _ type size) (begin (type-expr type) (if (whole-number? (type-expr size)) 
                                                          (atype type)   
                                                          (c-errorf "Array declaration expects numeric type for size")))]
      [(methodcall _ left _ args) 
       (let* ([left-env (cond
                          [(empty? left) (envs-types env)]           ;if the left is empty, use the current class env
                          [else (get-expr-envs all-cinfo left)])]    ;else use the rtype of the left to get the root env for all-cinfo 
              [method-funt (methodcall->funt ast type-expr)])
              
         (match (assoc  method-funt left-env)
           [(list a b) b]
           [_ (c-errorf "No Function of that name")]))]
           
      [(methoddecl _ id parameters) (error "Attempt to type Method Declaration")]
      [(method _ _ _ _ _ body) (type-expr body)]
      [(or (class _ _ _ _ _ _ body)
           (interface _ _ _ _ _ body)) (type-expr body)]
      [(cunit _ _ body) (type-expr body)]
    
      [(fieldaccess _ _ field) 
       (match (assoc field (envs-types env))
         [#f (c-errorf "Unbound Field Access")]
         [(list a b) b])]
    
      [(classcreate e class params) (error "Classcreate not implemented")]
      [(constructor e scope methoddecl body) (type-expr body)]
      [(keyword e _) (error "keyword not implemented")]
    
    
    
      [_ (error "Type Checker Not Implemented")]))
  
  (for-each (lambda (cinfo) (type-expr (cunit-body (info-ast cinfo)))) all-cinfo))
     
                                                
    
