#lang racket

(require "ast-tree.rkt")
(require "environments.rkt")
(require "heirarchy-checker.rkt")

(provide type-expr ast)




;;castable? Symbol Symbol -> Boolean
(define (castable? t1 t2)
  true)


;;type-expr : ast -> (union ptype rtype atype)
(define (type-expr ast)


  (define (test-un-op op right)
    (cond
      [(symbol=? op '!) (if (type-ast=? (type-expr right) (ptype empty 'boolean)) (ptype empty 'boolean)
                            (error "! operator expects type boolean"))]
      [(symbol=? op '-) (if (type-ast=? (type-expr right) (ptype empty 'int)) (ptype empty 'int)
                            (error "- operator expects numeric type"))]
      
      [else (error "Unimplemented operator")]))
                        
                      
  (match ast
    [(literal _ type value) type]
    [(cast _ c expr) (if (castable? c (type-expr expr)) c (error "Invalid Cast"))]
    

    [(iff _ test tru fls) (if (begin  (type-expr tru) (type-expr fls) (type-ast=? test (ptype empty 'boolean))) (ptype empty 'void) (error "Type of Test not Boolean"))]
    
    
    [(while _ test body) (if (begin (type-expr body) (type-ast=? test (ptype empty 'boolean)))
                             (ptype empty 'void)
                             (error "While test not Boolean!"))]
    
    [(for _ init clause update body) (if (begin (type-expr init) (type-expr update) (type-expr body)
                                                (type-ast=? (type-expr clause) (ptype empty 'boolean)))
                                         
                                         (ptype empty 'void)
                                         (error "For test not Boolean!"))]
    
    [(unop _ op right) (test-un-op op right)]))
                                                
    