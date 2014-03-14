#lang racket

(require "errorf.rkt")
(require "class-info.rkt")
(require "ast-tree.rkt")
(require "environments.rkt")
(require "heirarchy-checker.rkt")

(provide type-check)

(define (same-package? n1 n2)
  (cond [(and (empty? n1) (empty? n2)) #t]
        [(empty? n1) #f]
        [(empty? n2) #f]
        [(and (empty? (rest n1)) (empty? (rest n2))) #t]
        [(equal? (first n1) (first n2)) (same-package? (rest n1) (rest n2))]
        [else #f]))

(define (remove-last l)
  (reverse (rest (reverse l))))

(define (get-type-method C mod F all-cinfo methcall-ast)
  (define cenv (info-env (find-info C all-cinfo)))

  (define (get-rt-env rt)
    (match rt 
      [(rtype t) (info-env (find-info t all-cinfo))]
      [_ (c-errorf "Expression does not resolve to a class type.")]))

  (define (type-method rt)
    (define rt-env (get-rt-env rt))
    (define meth-funt (methodcall->funt methcall-ast F))
    (match (assoc meth-funt (envs-types rt-env))
      [(list a b) (cond
                    [(or (method-check? F method-mod (list 'static) methcall-ast rt-env)
                         (method-check? F method-mod (list 'static 'native) methcall-ast rt-env)) (c-errorf "Must NOT be a static method.")]
                    [(method-check? F method-scope 'public methcall-ast rt-env) b]
                    [(and (method-check? F method-scope 'protected methcall-ast rt-env)
                          (or (check-protected all-cinfo C rt (envs-methods cenv) (envs-methods rt-env) meth-funt )
                              (equal? (remove-last C) (remove-last (rtype-type rt))))) b]
                    [else (c-errorf "Trying to access method that is not public.")])]
      [_ (c-errorf "No Function of that name")]))

  (define (type-static-method rt)
    (define rt-env (get-rt-env rt))
    (define meth-funt (methodcall->funt methcall-ast F))
    (match (assoc meth-funt (envs-types rt-env))
      [(list a b) (cond
                    [(and (not (method-check? F method-mod (list 'static) methcall-ast rt-env))
                         (not (method-check? F method-mod (list 'static 'native) methcall-ast rt-env))) (c-errorf "Must be a static method.")]
                    [(method-check? F method-scope 'public methcall-ast rt-env) b]
                    [(and (method-check? F method-scope 'protected methcall-ast rt-env)
                          (or (check-protected all-cinfo C rt (envs-methods cenv) (envs-methods rt-env) meth-funt)
                              (superclass? all-cinfo C (rtype-type rt))
                              (equal? (remove-last C) (remove-last (rtype-type rt))))) b]
                    [else (c-errorf "Trying to access method that is not public.")])]
      [_ (c-errorf "No Function of that name")]))

  (define left (methodcall-left methcall-ast))
  (cond
    [(empty? left) (let* ([is-static (method-check? F method-mod `(static) methcall-ast (info-env (find-info C all-cinfo)))])
                     (cond
                       [(and (equal? mod (list 'static)) (not is-static)) (c-errorf "Cannot call non-static method inside static method.")]
                       [is-static (c-errorf "Calls a static method without naming the class.")]
                       [else (type-method (rtype C))]))]
    [(rtype? left) (type-static-method left)]
    [(and (this? left) (equal? mod '(static)))  (c-errorf "Cannot use \"this\" inside static method/initializer.")]
    [else (type-method (F left))]))

(define (check-protected all-cinfo C rt cenv rt-env field)
  (define (get-possible-scope field env)
    (define asoc (assoc field env))
    (cond
      [(false? asoc) #f]
      [else (eval-scope (second asoc))]))
  (define is-subclass (superclass? all-cinfo (rtype-type rt) C))
  (define c-field-scope (get-possible-scope field cenv))
  (define rt-field-scope (get-possible-scope field rt-env))
  (and is-subclass (equal? c-field-scope rt-field-scope)))

;;get-type-field
(define (get-type-field C mod F all-cinfo field-ast R/W)
  (define cenv (info-env (find-info C all-cinfo)))
  (define (get-rt-env rt)
    (match rt 
      [(rtype t) (info-env (find-info t all-cinfo))]
      [_ (c-errorf "Expression does not resolve to a class type.")]))

  (define (type-fieldaccess rt)
    (printf "HERE AGAIN... ~a~n" rt)
    (printf "HERE AGAIN... ~a~n" field-ast)
    (define rt-env (get-rt-env rt))
    (printf "HERE AGAIN... ~a~n" rt-env)
    (define field (fieldaccess-field field-ast))
    (match (assoc field (envs-types rt-env))
      [(list a b) (cond
                    [(field-check? F vdecl-mod 'static field-ast rt-env) (c-errorf "Must NOT be a static field access.")]
                    [(field-check? F vdecl-scope 'public field-ast rt-env) b]
                    [(and (field-check? F vdecl-scope 'protected field-ast rt-env)
                          (or (check-protected all-cinfo C rt (envs-vars cenv) (envs-vars rt-env) field)
                              (equal? (remove-last C) (remove-last (rtype-type rt))))) b]
                    [else (c-errorf "Trying to access field that is not public.")])]
      [_ (c-errorf "No Field of that name")]))

  (define (type-static-fieldaccess rt)
    (define rt-env (get-rt-env rt))
    (define field (fieldaccess-field field-ast))
    (match (assoc field (envs-types rt-env))
      [(list a b) (cond
                    [(not (field-check? F vdecl-mod 'static field-ast rt-env)) (c-errorf "Must be a static field access.")]
                    [(field-check? F vdecl-scope 'public field-ast rt-env) b]
                    [(and (field-check? F vdecl-scope 'protected field-ast rt-env)
                          (or (check-protected all-cinfo C rt (envs-vars cenv) (envs-vars rt-env) field)
                              (superclass? all-cinfo C (rtype-type rt))
                              (equal? (remove-last C) (remove-last (rtype-type rt))))) b]
                    [else (c-errorf "Trying to access field that is not public.")])]
      [_ (c-errorf "No Field of that name")]))

  (define left (fieldaccess-left field-ast))
  (cond
    [(this? left) (cond
                    [(equal? mod (list 'static)) (c-errorf "Cannot use \"this\" inside static method/initializer.")]
                    [else (type-fieldaccess (F left))])]
    [(rtype? left) (type-static-fieldaccess left)]
    [(and (atype? (F left)) (equal? "length" (fieldaccess-field field-ast))) (cond [(equal? R/W 'Read) (ptype 'int)]
                                                                                   [(equal? R/W 'Write) (c-errorf "cant write to final field")])]
    [else (type-fieldaccess (F left))]))

(define (check-varuse-static mod ast id)
  (define (check-asoc id-asoc)
    (define id-vdecl (if (varassign? (eval-ast (second asoc))) 
                         (varassign-id (eval-ast (second asoc)) )
                         (eval-ast (second asoc))))
    (and (or (equal? 'static mod) (equal? `(static) mod))
         (not (eval-local? (second asoc)))
         (not (equal? 'static (vdecl-mod id-vdecl)))))

  (printf "NICK HERE!~n")
  (print-ast ast "")
  (printf "ENV!~n")
  (envs-print (ast-env ast))
  (define asoc (assoc id (envs-vars (ast-env ast))))
  (cond
    [(false? asoc) #f]
    [(check-asoc asoc) (c-errorf "Use of non-static field variable inside a static method.")]
    [else #t]))

;;type-check : (assoc fullq-names info) -> void
(define (type-check all-cinfo) 
  
  ;;parent-of? rtype rtype envs -> Boolean
  (define (parent-of? T S)
    (superclass? all-cinfo (rtype-type S) (rtype-type T)))
  
  
  ;;num-type<? : ptype ptype -> Boolean
  (define (num-type<? pt1 pt2)
    (printf "num-type<? ~a ~a~n" pt1 pt2) 
    (match (list (ptype-type pt1) (ptype-type pt2))
      [(list 'byte t) (list? (member t '(short int char long float double)))]
      [(list 'short t) (list? (member t '(int char long float double)))]
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
    (printf "Castable? T: ~a S: ~a~n" T S)
    (match (list T S)
      [(list (or (atype _) (rtype _)) (ptype 'null)) #t]
      [(list (ptype sym1) (ptype sym2)) (cast-ptypes T S)]
      [(list (atype typ1) (atype typ2)) (castable? typ1 typ2 env)]
      [(list (or (atype _) (rtype _)) (or (atype _) (rtype _))) (if (type-ast=? T S) #t (or (can-assign? T S) (can-assign? S T)))]
      [(list _ _) #f]))
  
  
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
    (printf "super-interface? T: ~a S: ~a~n" T S)
    (define S-interfaces-1 (info-impls (find-info (rtype-type S) all-cinfo)))
    (define S-interfaces (if (list? S-interfaces-1) S-interfaces-1 (list S-interfaces-1)))
    (printf "S-interfaces: ~a~n" S-interfaces)
    (list? (member (rtype-type T) S-interfaces)))
  
  ;;rtype-can-assign? rtype rtype C-Lpat -> Boolean
  ;;checks to see if source rtype (S) can be assigned to target rtype (T)
  (define (rtype-can-assign? T S)
    (printf "rtype-can-assign? ~a ~a~n" T S)
    (cond
      [(class-type? S) (if (class-type? T) (parent-of? T S)  (super-interface? T S))]
      [(class-type? T)  (printf "~a is of class type~n" T) (type-ast=? T (rtype '("java" "lang" "Object")))]
      [else  (or (type-ast=? T S) (super-interface? T S))]))
  
  
  ;;can-assign? (union ptype rtype atype) (union ptype rtype atype) -> Boolean
  (define (can-assign? T S)
    (printf "can-assign? ~a ~a~n" T S)
    (match (list T S)
      [(list (ftype t1) (ftype t2)) (can-assign? t1 t2)]
      [(list (ftype t1) t2) (can-assign? t1 t2)]
      
      ;;can assign null to rtype
      [(list (or (atype _) (rtype _)) (ptype 'null)) #t]
   
      ;;can assign bool to bool
      [(list (ptype 'boolean) (ptype 'boolean)) #t]
      
      ;;cannot assign bool to any other ptype
      [(list (ptype 'boolean) (ptype _)) #f]
      [(list (ptype _) (ptype 'boolean)) #f]
      
      ;;If not Boolean, we know the ptypes are numeric. The following 4 are the only ptype assigns we allow
      [(list (ptype 'int) (ptype 'char)) #t]
      [(list (ptype 'int) (ptype 'short)) #t]
      [(list (ptype 'short) (ptype 'byte)) #t]
      [(list (ptype 'int) (ptype 'byte)) #t]
      [(list (ptype _) (ptype 'null)) #f]
      [(list (ptype _) (ptype _)) (printf "doing magic here ~a~n" (type-ast=? T S))
                                  (type-ast=? T S)]
     ; [(list (ptype _) (ptype _)) (not (num-type<? T S))]
      ;;Cannot assign ptype to an rtype, except null
      [(list (rtype _) (ptype _)) #f]
      ;;cannot assign null to a ptype
      
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
  (define (type-expr C mrtn mod ast)
    (printf "HERRERERE: ~n")
    (ast-print-struct ast)


     ;;perform-bin-op: symbol (union rtype ptype atype) (union rtype ptype atype) -> (union rtype ptype atype)
    (define (perform-bin-op op t1 t2)
      (match (list op t1 t2)
        ;;Special case: Can apply + operator to String/bool, bool/String and String/String:
        [(list 'plus (rtype '("java" "lang" "String")) (rtype '("java" "lang" "String"))) (rtype '("java" "lang" "String"))]
        [(list 'plus (rtype '("java" "lang" "String")) (ptype 'void)) (c-errorf "Undefined Binop ~a for types ~a ~a" op t1 t2)]
        [(list 'plus (ptype 'void) (rtype '("java" "lang" "String"))) (c-errorf "Undefined Binop ~a for types ~a ~a" op t1 t2)]
        [(list 'plus (rtype '("java" "lang" "String")) (or (rtype _) (atype _) (ptype _))) (rtype '("java" "lang" "String"))]
        [(list 'plus (or (rtype _) (atype _) (ptype _)) (rtype '("java" "lang" "String"))) (rtype '("java" "lang" "String"))]
        
        ;;Can apply == and != to bool/bool:
        [(list (or 'eqeq 'noteq 'barbar 'ampamp) (ptype 'boolean) (ptype 'boolean)) (ptype 'boolean)]
        
        ;;cann apply == and != to reference/reference:
        [(list (or 'eqeq 'noteq 'instanceof) (or (ptype 'null) (atype _) (rtype _)) (or (ptype 'null) (atype _) (rtype _))) (if (or (castable? t1 t2 (ast-env ast)) (castable? t2 t1 (ast-env ast))) (ptype 'boolean) (c-errorf "Attempt to perform equality operator ~a on non-castable types ~a ~a" op t1 t2))]
        
        ;;TODO: Verify that binops on two numerics behave like we think they do!
        [(list (or 'plus 'minus 'star  'slash 'pct) (ptype _) (ptype _)) (if (and (type-numeric? t1) (type-numeric? t2)) (ptype 'int) (c-errorf "Attempt to perform binary operation on non-numeric type ~a ~a ~a" op t1 t2))]
        [(list (or 'gt 'lt 'gteq 'lteq 'eqeq 'noteq) (ptype _) (ptype _))  (if (and (type-numeric? t1) (type-numeric? t2)) (ptype 'boolean) (c-errorf "Attempt to perform binary operation on non-numeric type ~a ~a ~a" op t1 t2))]
        [(list (or 'bar 'amp) _ _) (c-errorf "Bitwise operation detected: ~a" op)]
        [_ (c-errorf "Undefined Binop ~a for types ~a ~a" op t1 t2)]))
    
    (define (test-specific-bin-op type left right err-string)
      (if (and (type-ast=? type (type-expr C mrtn mod left)) (type-ast=? type (type-expr C mrtn mod right))) type (error err-string)))
    
    (define (test-un-op op right)
      (cond
        [(symbol=? op 'not) (if (type-ast=? (type-expr C mrtn mod right) (ptype 'boolean)) (ptype 'boolean)
                              (c-errorf "! operator expects type boolean"))]
        [(symbol=? op 'minus) (if (type-numeric? (type-expr C mrtn mod right)) (type-expr C mrtn mod right)
                              (c-errorf "- operator expects numeric type"))]))
                        
                      
    (match ast
      [(this _ type) type]
      [(vdecl _ _ _ type _) type]
    
      
      [(varassign _ (fieldaccess _ left field) val)
       (let ([field-type (get-type-field C mod (curry type-expr C mrtn mod) all-cinfo (varassign-id ast) 'Write)])
         (if (can-assign? field-type (type-expr C mrtn mod val))
             (if (ftype? field-type) (ftype-type field-type) field-type)
             (c-errorf "Type Mismatch in Assignment ~a ~a" field-type (type-expr C mrtn mod val))))]
      
      [(varassign _ id expr)
       (let ([var-type (type-expr C mrtn mod id)]
             [new-mod (if(vdecl? id) (vdecl-mod id) mod)])
         (if (can-assign? var-type (type-expr C mrtn new-mod expr))
             (if (ftype? var-type) (ftype-type var-type) var-type)
             (c-errorf "Type Mismatch in Assignment ~a ~a" var-type (type-expr C mrtn mod expr))))]
    
      [(varuse _ id)
       (check-varuse-static mod ast id)
       (match (assoc id (envs-types (ast-env ast)))
         [#f (c-errorf "Unbound Identifier")]
         [`(,x ,y) (if(class? y) (c-errorf "Unbound Identifier") y)])]
    
      [(literal _ type _) type]
      ;[(or (rtype '("java" "lang" "Integer"))) (ptype 'int)]
      [(or
        (ptype _) (atype _ ) (rtype  _)) ast]
    
      [(cast _ c expr) 
         (if (castable? (type-expr C mrtn mod c) (type-expr C mrtn mod expr) (ast-env ast)) (type-expr C mrtn mod c) (c-errorf "Invalid Cast ~a ~a" (type-expr C mrtn mod c) (type-expr C mrtn mod expr)))]
    
      [(iff _ test tru fls) (if (begin 
                                  (if (not (empty? tru)) (type-expr C mrtn mod tru) (printf "na")) 
                                  (if (not (empty? fls)) (type-expr C mrtn mod fls) (printf "na")) 
                                  (type-ast=? (type-expr C mrtn mod test) (ptype 'boolean)))
                            (ptype 'void) 
                            (c-errorf "Type of Test not Boolean" ))]
    
    
      [(while _ test body) (if (begin (type-expr C mrtn mod body) (type-ast=? (type-expr C mrtn mod test) (ptype 'boolean)))
                               (ptype 'void)
                               (c-errorf "While test not Boolean!"))]
    
      [(for _ init clause update body) (if (begin (run-nonempty (curry type-expr C mrtn mod) init) 
                                                  (run-nonempty (curry type-expr C mrtn mod) update)
                                                  (type-expr C mrtn mod body)
                                                  (let* ([clausecheck (run-nonempty (curry type-expr C mrtn mod) clause)]
                                                         [clausetype (if (empty? clausecheck) (ptype 'boolean) clausecheck)])
                                                    (type-ast=? clausetype (ptype 'boolean))))
                                           (ptype 'void)
                                           (c-errorf "For test not Boolean!"))]
     
      [(unop _ op right) (test-un-op op (type-expr C mrtn mod right))]
      [(binop _ op left right) (perform-bin-op op (type-expr C mrtn mod left) (type-expr C mrtn mod right))]
      [(parameter _ type _) type]
    
    
      [(block _ _ statements) (begin (map (curry type-expr C mrtn mod) statements) (ptype 'void))]
      [(arrayaccess _ left index) (if (whole-number? (type-expr C mrtn mod index)) 
                                      (if (atype? (type-expr C mrtn mod left)) 
                                          (atype-type (type-expr C mrtn mod left))
                                          (c-errorf "Array type expected")) 
                                      (c-errorf "Array index expects type int"))]
      [(return _ `()) (ptype 'void)]
      [(return _ expr) (let* ([rtn-type (type-expr C mrtn mod expr)])
                         (cond
                           [(equal? rtn-type (ptype 'void)) (c-errorf "Method return cannot return type void.")]
                           [(not (can-assign? mrtn rtn-type)) (c-errorf "Return type \"~a\" of method is not equal to a return statements return type \"~a\"." mrtn rtn-type)]
                           [else rtn-type]))]
      [(arraycreate _ type size) (begin (type-expr C mrtn mod type) (if (whole-number? (type-expr C mrtn mod size)) 
                                                          (atype (type-expr C mrtn mod type))   
                                                          (c-errorf "Array declaration expects numeric type for size")))]
      [(methodcall _ left _ args) (get-type-method C mod (curry type-expr C mrtn mod) all-cinfo ast)]
           
      [(methoddecl _ id parameters) (error "Attempt to type Method Declaration")]
      [(method _ _ mod t _ body) (type-expr C t mod body)]
      [(or (class _ _ _ _ _ _ body)
           (interface _ _ _ _ _ body)) (type-expr C mrtn mod body)]
      [(cunit _ _ body) (type-expr C mrtn mod body)]
    
      [(fieldaccess _ left field) (get-type-field C mod (curry type-expr C mrtn mod) all-cinfo ast 'Read)]
    
      [(classcreate e class params) (let ([confunt (funt "" (map (curry type-expr C mrtn mod) params))]
                                          [class-consts (envs-constructors (info-env (find-info (rtype-type class) all-cinfo)))]
                                          [class-ast (info-ast (find-info (rtype-type class) all-cinfo))])
                                      (cond [(is-class-with-mod? class-ast 'abstract) (c-errorf "Trying to instance abstract classs ~a" (rtype-type class))]
                                            [(not (is-class? class-ast)) (c-errorf "trying to instance non-class ~a" (rtype-type class))])
                                            (match (assoc confunt class-consts)
                                              [`(,_ ,(eval _ _ (constructor _ `public _ _))) class]
                                              [`(,_ ,(eval _ _ (constructor _ `protected _ _))) (if (same-package? (rtype-type class) C)
                                                                                                    class
                                                                                                    (c-errorf "Invalid call to protected constructor of class ~a from ~a" class C))]
                                              [_ (c-errorf "~a constructor type not found ~a" (string-join (rtype-type class) ".") confunt)]))]
      
      [(constructor e scope methoddecl body) (type-expr C mrtn mod body)]
    
      [_ (error "Type Checker Not Implemented")]))

  (for-each (lambda (cinfo) 
              (printf "###### TYPE CHECKING ~a ####~n" (info-name cinfo)) 
              (print-ast (info-ast cinfo) "") 
              (envs-print (info-env cinfo)) 
              (type-expr (info-name cinfo) empty empty (cunit-body (info-ast cinfo)))) all-cinfo))

     
                                                
    
