#lang racket

(require "ast-tree.rkt")
(require "environments.rkt")
(require "heirarchy-checker.rkt")

(provide type-expr ast)



;;parent-of? rtype rtype -> Boolean
(define (parent-of? T S)
  '())

;;castable? (union ptype rtype atype) (union ptype rtype atype) envs -> Boolean
(define (castable? t1 t2 env)
  (error "Castable not implemented"))


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
    [(varuse env 'this) (error "This not implemented")]
    [(vdecl _ _ _ _ _) (ptype empty 'void)]
    
    [(varassign env id expr)
     (let ([var-type (type-expr id)])
       (if (type-ast=? var-type (type-expr expr))
           var-type
           (error "Type Mismatch in Assignment")))]
    
    [(varuse env id)
     (match (assoc id (envs-types env))
       [#f (error "Unbound Identifier")]
       [(list a b) b])]
    
    [(literal _ type value) type]
    [(or
      (ptype _ _ _) (atype _ _ _) (rtype _ _ _)) ast]
    [(cast env c expr) (if (castable? c (type-expr expr) env) c (error "Invalid Cast"))]
    
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
    [(parameter env type id) (error "parameter not implemented")]
    
    ;[`(,binop _ + ,left ,right) '()]
    [(block _ _ statements) (begin (map type-expr statements) (ptype empty 'void))]
    [(arrayaccess _ left index) (if (whole-number? (type-expr index)) 
                                    (if (atype? (type-expr left)) 
                                        (atype-type (type-expr left)) 
                                        (error "Array type expected")) 
                                    (error "Array index expects type int"))]
    [(return _ expr) (type-expr expr)]
    [(arraycreate _ type size) (begin (type-expr type) (if (whole-number? (type-expr size)) (atype type) (error "Array declaration expects integer size")))]
    ;[(classcreate _ class params) (begin (map type-expr params)
    [(methodcall env left args) (error "Methodcall not implemented")]
    [(methoddecl env id parameters) (error "Methoddecl not implemented")]
    [(method _ _ _ _ body) (type-expr body)]
    [(or (class _ _ _ _ _ _ body)
         (interface _ _ _ _ _ body)) (type-expr body)]
    [(cunit _ _ body) (type-expr body)]
    [(fieldaccess env left field) (error "Fieldaccess not implemented")]
    [(classcreate env class params) (error "Classcreate not implemented")]
    [(constructor env scope methoddecl body) (type-expr body)]
    [(keyword env id) (error "keyword not implemented")]
    
    
    
    [_ (error "Type Checker Not Implemented")]))

     
                                                
    