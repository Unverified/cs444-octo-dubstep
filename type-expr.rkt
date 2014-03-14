#lang racket

(require "errorf.rkt")
(require "class-info.rkt")
(require "ast-tree.rkt")
(require "environments.rkt")
(require "heirarchy-checker.rkt")

(provide type-check)

(define (get-type-method C F all-cinfo methcall-ast)
  (define (get-rt-env rt)
    (match rt 
      [(rtype t) (info-env (find-info t all-cinfo))]
      [_ (c-errorf "Expression does not resolve to a class type.")]))

  (define (type-method rt)
    (define rt-env (get-rt-env rt))
    (define meth-funt (methodcall->funt methcall-ast F))
    (match (assoc meth-funt (envs-types rt-env))
      [(list a b) (cond
                    [(method-check? F method-scope 'public methcall-ast rt-env) b]
                    [(and (method-check? F method-scope 'protected methcall-ast rt-env)
                          (superclass? all-cinfo (rtype-type rt) C)) b]
                    [else (c-errorf "Trying to access method that is not public.")])]
      [_ (c-errorf "No Function of that name")]))

  (define (type-static-method rt)
    (define meth-ret-t (type-method rt))
    (define rt-env (get-rt-env rt))
    (cond
      [(method-check? F method-mod (list 'static) methcall-ast rt-env) meth-ret-t]
      [(method-check? F method-mod (list 'static 'native) methcall-ast rt-env) meth-ret-t]
      [else (c-errorf "Trying to access a method in ~a that is not static. ~a" (rtype-type rt) methcall-ast)]))

  (define left (methodcall-left methcall-ast))
  (cond
    [(empty? left) (type-method (rtype C))]
    [(rtype? left) (type-static-method left)]
    [else (type-method (F left))]))

;;get-type-field
(define (get-type-field C F all-cinfo field-ast)
  (printf "IS THIS A VARASUDSA: ~a~n" field-ast)
  (define (get-rt-env rt)
    (match rt 
      [(rtype t) (info-env (find-info t all-cinfo))]
      [_ (c-errorf "Expression does not resolve to a class type.")]))

  (define (type-fieldaccess rt)
    (define rt-env (get-rt-env rt))
    (define field (fieldaccess-field field-ast))
    (match (assoc field (envs-types rt-env))
      [(list a b) (cond
                    [(field-check? F vdecl-scope 'public field-ast rt-env) b]
                    [(and (field-check? F vdecl-scope 'protected field-ast rt-env)
                          (superclass? all-cinfo (rtype-type rt) C)) b]
                    [else (c-errorf "Trying to access field that is not public.")])]
      [_ (c-errorf "No Field of that name")]))

  (define (type-static-fieldaccess rt)
    (define field-ret-t (type-fieldaccess rt))
    (define rt-env (get-rt-env rt))
    (cond
      [(field-check? F vdecl-mod 'static field-ast rt-env) field-ret-t]
      [else (c-errorf "Trying to access a field in ~a that is not static. ~a" (rtype-type rt) field-ast)]))

  (define left (fieldaccess-left field-ast))
  (define ty (if (rtype? left) left (F left)))
  (printf "ty: ~a~n" ty)
  (cond
    [(rtype? ty) (type-static-fieldaccess ty)]
    [(and (atype? ty) (equal? "length" (fieldaccess-field field-ast))) (ptype 'int)]
    [else (type-fieldaccess ty)]))

;;type-check : (assoc fullq-names info) -> void
(define (type-check all-cinfo)
  
  
  ;;perform-bin-op: symbol (union rtype ptype atype) (union rtype ptype atype) -> (union rtype ptype atype)
  (define (perform-bin-op op t1 t2)
    (match (list op t1 t2)
      ;;Special case: Can apply + operator to String/bool, bool/String and String/String:
      [(list 'plus (rtype '("java" "lang" "String")) (rtype '("java" "lang" "String"))) (rtype '("java" "lang" "String"))]
      [(list 'plus (rtype '("java" "lang" "String")) (ptype 'boolean)) (rtype '("java" "lang" "String"))]
      [(list 'plus (ptype 'boolean) (rtype '("java" "lang" "String"))) (rtype '("java" "lang" "String"))]
      
      ;;Can apply == and != to bool/bool:
      [(list (or 'eqeq 'noteq 'barbar 'ampamp) (ptype 'boolean) (ptype 'boolean)) (ptype 'boolean)]
      
      ;;cann apply == and != to reference/reference:
      [(list (or 'eqeq 'noteq 'instanceof) (or (ptype 'null) (atype _) (rtype _)) (or (ptype 'null) (atype _) (rtype _))) (ptype 'boolean)]
      
      ;;TODO: Verify that binops on two numerics behave like we think they do!
      [(list (or 'plus 'minus 'star  'slash 'pct) (ptype _) (ptype _)) (if (and (type-numeric? t1) (type-numeric? t2)) (ptype 'int) (c-errorf "Attempt to perform binary operation on non-numeric type ~a ~a ~a" op t1 t2))]
      [(list (or 'gt 'lt 'gteq 'lteq 'eqeq 'noteq) (ptype _) (ptype _))  (if (and (type-numeric? t1) (type-numeric? t2)) (ptype 'boolean) (c-errorf "Attempt to perform binary operation on non-numeric type ~a ~a ~a" op t1 t2))]
      [(list (or 'bar 'amp) _ _) (c-errorf "Bitwise operation detected: ~a" op)]
      [_ (c-errorf "Undefined Binop ~a for types ~a ~a" op t1 t2)]))
  
  ;;parent-of? rtype rtype envs -> Boolean
  (define (parent-of? T S)
    (superclass? all-cinfo (rtype-type S) (rtype-type T)))
  
  
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
  

  ;;num-type-min : ptype ptype -> ptype
  ;;May be useful if numerics turn out to be more complicated than we think
  (define (num-type-min p1 p2)
    (if (num-type<? p1 p2) p1 p2))

  ;;num-type-max : ptype ptype -> ptype
  ;;
  (define (num-type-max p1 p2)
    (if (num-type<? p1 p2) p2 p1))
  
  
  
  ;;cast-ptypes : ptype ptype -> Boolean
  (define (cast-ptypes T S)
    (match (list T S)
      ;;identity conversions: bool to bool
      [(list (ptype 'boolean) (ptype 'boolean)) #t]
      ;;invalid conversions: bool to anything else
      [(list (ptype 'boolean) _) #f]
      [(list _ (ptype 'boolean)) #f]
      
      ;;remaining conversions: Numeric type to numeric type. Will work out widening/narrowing later.
      [(list (ptype _) (ptype _)) (and (type-numeric? T) (type-numeric? S))]
      
      [_ (c-errorf "Unimplemented ptype cast! ~a ~a" T S)]))
  
  ;;castable? (union ptype rtype atype) (union ptype rtype atype) envs -> Boolean
  (define (castable? T S env)
    (match (list T S)
      [(list (ptype sym1) (ptype sym2)) (cast-ptypes T S)]
      [(list (atype typ1) (atype typ2)) (castable? typ1 typ2)]
      [(list (or (atype _) (rtype _)) (or (atype _) (rtype _))) (if (type-ast=? T S) #t (or (can-assign? T S) (can-assign? S T)))]
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
  
  
  ;;class-type? rtype -> Boolean
  (define (class-type? r)
    (list? (info-impls (find-info (rtype-type r) all-cinfo))))
     
  
  ;;super-interface? rtype rtype -> Boolean
  (define (super-interface? T S)
    (define S-interfaces (info-impls (find-info (rtype-type S) all-cinfo)))
    (list? (member (rtype-type T) S-interfaces)))
  
  ;;rtype-can-assign? rtype rtype C-Lpat -> Boolean
  ;;checks to see if source rtype (S) can be assigned to target rtype (T)
  (define (rtype-can-assign? T S)
    (cond
      [(class-type? S) (parent-of? S T)]
      [else (if (class-type? T) 
                (type-ast=? T (rtype '("java" "lang" "Object")))
                (super-interface? S T))]))
  
  
  ;;can-assign? (union ptype rtype atype) (union ptype rtype atype) -> Boolean
  (define (can-assign? T S)
    (match (list T S)
      ;;can assign null to rtype
      [(list (rtype _) (ptype 'null)) #t]
      
      ;;can assign bool to bool
      [(list (ptype 'boolean) (ptype 'boolean)) #t]
      
      ;;cannot assign bool to any other ptype
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
      [(list (or (rtype '("java" "lang" "Object"))
                 (rtype '("java" "io" "Serializable"))
                 (rtype '("java" "lang" "Cloneable")))
             (atype _)) #t]
      
      ;;assigning atypes to any but the three above results in a compile-time error
      [(list (rtype _) (atype _)) #f]
      
      ;;assigning an atype of ptypes to an atype of ptypes requires the ptypes be equal
      [(list (atype (ptype _)) (atype (ptype _))) (type-ast=? (atype-type T) (atype-type S))]
      
      ;;assigning an atype of rtypes to an atype of rtypes requires the rtypes to be assignable
      [(list (atype (rtype _)) (atype (rtype _))) (can-assign? (atype-type T) (atype-type S))]
      
      
      
      ;;any other assignment involving atypes is a compile-time error
      [(list (atype _) (atype _)) #f]                                                     
      
      [_ #f]))
  
 ;;type-expr : ast -> (union ptype rtype atype)
  (define (type-expr C ast)
    (ast-print-struct ast)
    (define (test-specific-bin-op type left right err-string)
      (if (and (type-ast=? type (type-expr C left)) (type-ast=? type (type-expr C right))) type (error err-string)))

    (define (test-un-op op right)
      (cond
        [(symbol=? op 'not) (if (type-ast=? (type-expr C right) (ptype 'boolean)) (ptype 'boolean)
                              (error "! operator expects type boolean"))]
        [(symbol=? op 'minus) (if (type-numeric? (type-expr C right)) (type-expr C right)
                              (error "- operator expects numeric type"))]
        [else (c-errorf "Unimplemented operator ~a" op)]))
                        
                      
    (match ast
      [(this _ type) type]
      [(vdecl _ _ _ type _) type]
    
      [(varassign _ id expr)
       (let ([var-type (type-expr C id)])
         (if (can-assign? var-type (type-expr C expr))
             var-type
             (c-errorf "Type Mismatch in Assignment ~a ~a" var-type (type-expr C expr))))]
    
      [(varuse _ id)
       (match (assoc id (envs-types (ast-env ast)))
         [#f (c-errorf "Unbound Identifier")]
         [(list a b) (printf "VARUSE IS: ~a~n" b) b])]
    
      [(literal _ type _) type]
      [(or
        (ptype _) (atype _ ) (rtype  _)) ast]
    
      [(cast _ c expr) 
         (if (castable? c (type-expr C expr) (ast-env ast)) c (c-errorf "Invalid Cast"))]
    
      [(iff _ test tru fls) (if (begin 
                                  (if (not (empty? tru)) (type-expr C tru) (printf "na")) 
                                  (if (not (empty? fls)) (type-expr C fls) (printf "na")) 
                                  (type-ast=? (type-expr C test) (ptype 'boolean)))
                            (ptype 'void) 
                            (c-errorf "Type of Test not Boolean" ))]
    
    
      [(while _ test body) (if (begin (type-expr C body) (type-ast=? (type-expr C test) (ptype 'boolean)))
                               (ptype 'void)
                               (c-errorf "While test not Boolean!"))]
    
      [(for _ init clause update body) (if (begin (type-expr C init) (type-expr C update) (type-expr C body)
                                                  (type-ast=? (type-expr C clause) (ptype 'boolean)))
                                         
                                           (ptype 'void)
                                           (c-errorf "For test not Boolean!"))]
     
      [(unop _ op right) (test-un-op op right)]
      [(binop _ op left right) (perform-bin-op op (type-expr C left) (type-expr C right))]
      [(parameter _ type _) type]
    
    
      [(block _ _ statements) (begin (map (curry type-expr C) statements) (ptype 'void))]
      [(arrayaccess _ left index) (if (whole-number? (type-expr C index)) 
                                      (if (atype? (type-expr C left)) 
                                          (atype-type (type-expr C left)) 
                                          (c-errorf "Array type expected")) 
                                      (c-errorf "Array index expects type int"))]
      [(return _ expr) (type-expr C expr)]
      [(arraycreate _ type size) (begin (type-expr C type) (if (whole-number? (type-expr C size)) 
                                                          (atype type)   
                                                          (c-errorf "Array declaration expects numeric type for size")))]
      [(methodcall _ left _ args) (get-type-method C (curry type-expr C) all-cinfo ast)]
           
      [(methoddecl _ id parameters) (error "Attempt to type Method Declaration")]
      [(method _ _ _ _ _ body) (type-expr C body)]
      [(or (class _ _ _ _ _ _ body)
           (interface _ _ _ _ _ body)) (type-expr C body)]
      [(cunit _ _ body) (type-expr C body)]
    
      [(fieldaccess _ left field) (get-type-field C (curry type-expr C) all-cinfo ast)]
    
      [(classcreate e class params) (let ([confunt (funt "" (map (curry type-expr C) params))]
                                          [class-consts (envs-constructors (info-env (find-info (rtype-type class) all-cinfo)))])
                                      (cond [(pair? (assoc confunt class-consts))  class]
                                            [else (c-errorf "~a constructor type not found ~a" (string-join (rtype-type class) ".") confunt)]))]
      
      [(constructor e scope methoddecl body) (type-expr C body)]
    
      [_ (error "Type Checker Not Implemented")]))

  (for-each (lambda (cinfo) (printf "###### TYPE CHECKING ~a ####~n" (info-name cinfo)) (print-ast (info-ast cinfo) "") (type-expr (info-name cinfo) (cunit-body (info-ast cinfo)))) all-cinfo))

     
                                                
    
