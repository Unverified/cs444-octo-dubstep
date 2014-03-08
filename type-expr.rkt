#lang racket

(require "ast-tree.rkt")
(require "environments.rkt")
(require "heirarchy-checker.rkt")

(provide type-expr ast)



;;parent-of? rtype rtype envs -> Boolean
(define (parent-of? T S env)
  '())

;;castable? (union ptype rtype atype) (union ptype rtype atype) envs -> Boolean
(define (castable? t1 t2 env)
  (match (list t1 t2)
    [(list (ptype _ _) (ptype _ _)) (type-ast=? t1 t2)]
    [(list (atype _ typ1) (atype _ typ2)) (castable? typ1 typ2)]
    [(list (rtype _ _) (rtype _ _)) (if (type-ast=? t1 t2) #t (or (parent-of? t1 t2 env) (parent-of t2 t1 env)))]
    [(list _ _) (error "Cast type mismatch")]))


;;define whole-number? ptype
(define (whole-number? pt)
  (define valid-types '(int short char byte))
  (match pt
    [(ptype _ typ) (list? (member typ valid-types))]
    [_ #f]))

;;type-expr : ast -> (union ptype rtype atype)
(define (type-expr ast)

  (define (test-specific-bin-op type left right err-string)
    (if (and (type-ast=? type (type-expr left)) (type-ast=? type (type-expr right))) type (error err-string)))

  (define (test-un-op op right)
    (cond
      [(symbol=? op '!) (if (type-ast=? (type-expr right) (ptype empty 'boolean)) (ptype empty 'boolean)
                            (error "! operator expects type boolean"))]
      [(symbol=? op '-) (if (type-ast=? (type-expr right) (ptype empty 'int)) (ptype empty 'int)
                            (error "- operator expects numeric type"))]
      [else (error "Unimplemented operator")]))
                        
                      
  (match ast
    [(varuse e 'this) (error "This not implemented")]
    [(vdecl _ _ _ _ _) (ptype empty 'void)]
    
    [(varassign _ id expr)
     (let ([var-type (type-expr id)])
       (if (type-ast=? var-type (type-expr expr))
           var-type
           (error "Type Mismatch in Assignment")))]
    
    [(varuse e id)
     (let ([env (ast-envt ast)])
     (match (assoc id (envs-types env))
       [#f (error "Unbound Identifier")]
       [(list a b) b]))]
    
    [(literal _ type value) type]
    [(or
      (ptype _ _ _) (atype _ _ _) (rtype _ _ _)) ast]
    
    
    [(cast e c expr) 
     (let ([env (ast-envt ast)])
       (if (castable? c (type-expr expr) env) c (error "Invalid Cast")))]
    
    [(iff _ test tru fls) (if (begin  (type-expr tru) (type-expr fls) (type-ast=? test (ptype empty 'boolean))) (ptype empty 'void) (error "Type of Test not Boolean"))]
    
    
    [(while _ test body) (if (begin (type-expr body) (type-ast=? test (ptype empty 'boolean)))
                             (ptype empty 'void)
                             (error "While test not Boolean!"))]
    
    [(for _ init clause update body) (if (begin (type-expr init) (type-expr update) (type-expr body)
                                                (type-ast=? (type-expr clause) (ptype empty 'boolean)))
                                         
                                         (ptype empty 'void)
                                         (error "For test not Boolean!"))]
    
    [(unop _ op right) (test-un-op op right)]
    [(binop _ _ _ _) (error "BinOp not implemented")]
    [(parameter e type id) (error "parameter not implemented")]
    
    
    [(block _ _ statements) (begin (map type-expr statements) (ptype empty 'void))]
    [(arrayaccess _ left index) (if (whole-number? (type-expr index)) 
                                    (if (atype? (type-expr left)) 
                                        (atype-type (type-expr left)) 
                                        (error "Array type expected")) 
                                    (error "Array index expects type int"))]
    [(return _ expr) (type-expr expr)]
    [(arraycreate _ type size) (begin (type-expr type) (if (whole-number? (type-expr size)) (atype type) (error "Array declaration expects integer size")))]
    ;[(classcreate _ class params) (begin (map type-expr params)
    [(methodcall e left args) (error "Methodcall not implemented")]
    [(methoddecl e id parameters) (error "Methoddecl not implemented")]
    [(method _ _ _ _ body) (type-expr body)]
    [(or (class _ _ _ _ _ _ body)
         (interface _ _ _ _ _ body)) (type-expr body)]
    [(cunit _ _ body) (type-expr body)]
    [(fieldaccess e left field) (error "Fieldaccess not implemented")]
    [(classcreate e class params) (error "Classcreate not implemented")]
    [(constructor e scope methoddecl body) (type-expr body)]
    [(keyword e id) (error "keyword not implemented")]
    
    
    
    [_ (error "Type Checker Not Implemented")]))

     
                                                
    