#lang racket

(require "scanner.rkt")
(require "parse-tree.rkt")

(provide parse->ast)

;(struct c-unit ([package : (Listof String)] [imports : (Listof (Listof String))] [body : something]))
(struct cunit (package imports body) #:transparent)
(struct cimport (path) #:transparent)
(struct pimport (path) #:transparent)
(struct interface (scope mod id extends body) #:transparent)
(struct class (scope mod id extends implements body) #:transparent)
(struct constructor (scope method-decl body) #:transparent)
(struct method (scope mod type method-decl body) #:transparent)
(struct method-decl (id parameters) #:transparent)
(struct parameter (type id) #:transparent)
(struct var (scope mod type var-assign) #:transparent)
(struct varassign (id expr) #:transparent)
(struct binop (op left right) #:transparent)
(struct unop (op right) #:transparent)
(struct cast (c expr) #:transparent)
(struct arraycreate (type size) #:transparent)
(struct classcreate (class params) #:transparent)
(struct fieldaccess (left field) #:transparent)
(struct methodcall (left args) #:transparent)
(struct arrayaccess (left index) #:transparent)
(struct iff (test tru fls) #:transparent)
(struct while (test body) #:transparent)
(struct for (init clause update body) #:transparent)
(struct return (expr) #:transparent)
(struct ptype (type) #:transparent)
(struct rtype (type) #:transparent)
(struct atype (type) #:transparent)
(struct block (statements) #:transparent)

(define (parse->ast t)
  (match t
    [(tree (node _) '()) empty]
    [(tree (node 'S) `(,x ,y ,z)) (cunit (parse->ast x) (parse->ast y) (parse->ast z))]
    [(tree (node 'PACKAGE) `(,_ ,x ,_)) (parse->ast x)]
    [(tree (node 'IMPORTS) `(,x)) (parse->ast x)]
    [(tree (node 'IMPORT_LIST) `(,x ,y)) (append (parse->ast x) (list (parse->ast y)))]
    [(tree (node 'IMPORT_LIST) `(,x)) (list (parse->ast x))]
    [(tree (node 'IMPORT) `(,x)) (parse->ast x)]
    [(tree (node 'CLASS_IMPORT) `(,_ ,x ,_)) (cimport (parse->ast x))]
    [(tree (node 'PACKAGE_IMPORT) `(,_ ,x ,_ ,_ ,_)) (pimport (parse->ast x))]

    [(tree (node 'CLASS_OR_INTERFACE_DECLARATION) `(,x)) (parse->ast x)]
    [(tree (node 'INTERFACE_DECLARATION) x) (interface (parse->ast (list-ref x 0)) (parse->ast (list-ref x 1)) (parse->ast (list-ref x 3)) (parse->ast (list-ref x 4)) (parse->ast (list-ref x 5)))]
    [(tree (node 'CLASS_DECLARATION) x) (class (parse->ast (list-ref x 0)) (parse->ast (list-ref x 1)) (parse->ast (list-ref x 3)) (parse->ast (list-ref x 4)) (parse->ast (list-ref x 5)) (parse->ast (list-ref x 6)))]

    [(tree (node 'EXTENDS) `(,x ,y)) (parse->ast y)]
    [(tree (node 'IMPLEMENTS) `(,x ,y)) (parse->ast y)]

    [(tree (node 'IMPLEMENTS_TYPE_LIST) `(,x ,_ ,y)) (append (parse->ast x) (list (parse->ast y)))]
    [(tree (node 'IMPLEMENTS_TYPE_LIST) `(,x)) (list (parse->ast x))]

    [(tree (node 'INTERFACE_BODY_DECLARATIONS_OPT) `(,x)) (parse->ast x)]
    [(tree (node 'INTERFACE_BODY_DECLARATIONS) `(,x ,y)) (append (parse->ast x) (parse->ast y))]
    [(tree (node 'INTERFACE_BODY_DECLARATIONS) `(,x)) (parse->ast x)]
    [(tree (node 'INTERFACE_BODY_DECLARATION) `(,x)) (list (parse->ast x))]
    [(tree (node 'NORMAL_METHOD_DECLARATION_NO_BODY) `(,scope ,type ,decl ,body)) (method (parse->ast scope) empty (parse->ast type) (parse->ast decl) (parse->ast body))]

    [(tree (node 'CLASS_BODY_DECLARATIONS) `(,x ,y)) (append (parse->ast x) (parse->ast y))]
    [(tree (node 'CLASS_BODY_DECLARATIONS) `(,x)) (parse->ast x)]
    [(tree (node 'CLASS_BODY_DECLARATION) `(,x)) (list (parse->ast x))]

    [(tree (node 'NORMAL_METHOD_DECLARATION) `(,scope ,type ,decl ,body)) (method (parse->ast scope) empty (parse->ast type) (parse->ast decl) (parse->ast body))]
    [(tree (node 'ABSTRACT_METHOD_DECLARATION) `(,scope ,mod ,type ,decl ,body)) (method (parse->ast scope) (list (parse->ast mod)) (parse->ast type) (parse->ast decl) (parse->ast body))]
    [(tree (node 'FINAL_METHOD_DECLARATION) `(,scope ,mod ,type ,decl ,body)) (method (parse->ast scope) (list (parse->ast mod)) (parse->ast type) (parse->ast decl) (parse->ast body))]
    [(tree (node 'STATIC_METHOD_DECLARATION) `(,scope ,mod ,type ,decl ,body)) (method (parse->ast scope) (list (parse->ast mod)) (parse->ast type) (parse->ast decl) (parse->ast body))]
    [(tree (node 'STATIC_NATIVE_METHOD_DECLARATION) `(,scope ,mod ,type ,decl ,body)) (method (parse->ast scope) (parse->ast mod) (ptype (parse->ast type)) (parse->ast decl) (parse->ast body))]
    [(tree (node 'CONSTRUCTOR_DECLARATION) `(,scope ,decl ,body)) (constructor (parse->ast scope) (parse->ast decl) (parse->ast body))]

    [(tree (node 'STATIC_NATIVE) `(,x ,y)) (list (parse->ast x) (parse->ast x))]

    [(tree (node 'METHOD_DECLARATOR) `(,id ,_ ,params ,_)) (method-decl (parse->ast id) (parse->ast params))]
    [(tree (node 'STATIC_NATIVE_BODY) `(,id ,_ ,int ,p-id ,_)) (method-decl (parse->ast id) (list (parameter (parse->ast int) (parse->ast p-id))))]

    [(tree (node 'PARAMETER_LIST) `(,params ,_ ,param)) (append (parse->ast params) (parse->ast param))]
    [(tree (node 'PARAMETER_LIST) `(,param)) (parse->ast param)]
    [(tree (node 'PARAMETER) `(,type ,id)) (list (parameter (parse->ast type) (parse->ast id)))]

    [(tree (node 'INTERFACE_MODIFIER) `(,x)) (parse->ast x)]
    [(tree (node 'CLASS_MODIFIER) `(,x)) (parse->ast x)]

    [(tree (node 'TYPE) `(,x)) (parse->ast x)]
    [(tree (node 'PRIMITIVE_TYPE) `(,x)) (ptype (parse->ast x))]
    [(tree (node 'REFERENCE_TYPE) `(,x)) (rtype (parse->ast x))]
    [(tree (node 'ARRAY_TYPE) x) (atype (parse->ast (first x)))]

    [(tree (node 'CLASS_VARIABLE_DECLARATION) `( ,x ,type ,v ,_ )) (var (parse->ast x) empty (parse->ast type) (parse->ast v))]
    [(tree (node 'STATIC_CLASS_VARIABLE_DECLARATION) `( ,x ,_ ,type ,v ,_ )) (var (parse->ast x) 'static (parse->ast type) (parse->ast v))]
    [(tree (node 'VARIABLE_DECLARATOR_OPT) `( ,x )) (parse->ast x) ]
    [(tree (node 'VARIABLE_DECLARATOR) `( ,id ,_ ,expr )) (varassign (parse->ast id) (parse->ast expr)) ]

    [(tree (node 'IDS) `(,x ,z ,y)) (append (parse->ast x) (list (parse->ast y)))]
    [(tree (node 'IDS) `(,x)) (list (parse->ast x))]

    [(or (tree (node 'BLOCK) `(,_ ,x ,_))
         (tree (node 'PRIMARY_NO_NEW_ARRAY) `(,_ ,x ,_))
         (tree (node 'INTERFACE_BODY) `(,_ ,x ,_))
         (tree (node 'CLASS_BODY) `(,_ ,x ,_))) (parse->ast x)]

    [(or (tree (node 'EXPRESSION) `(,x))
         (tree (node 'ASSIGNMENT_EXPRESSION) `(,x)) 
         (tree (node 'LOGICAL_OR) `(,x))
         (tree (node 'LOGICAL_AND) `(,x))
         (tree (node 'BITWISE_OR) `(,x))
         (tree (node 'BITWISE_AND) `(,x))
         (tree (node 'EQUALITY) `(,x))
         (tree (node 'RELATIONAL) `(,x))
         (tree (node 'ADDITIVE) `(,x))
         (tree (node 'MULTIPLICATIVE) `(,x))
         (tree (node 'UNARY_MINUS) `(,x))
         (tree (node 'UNARY_NOT) `(,x))
         (tree (node 'LOGICAL_OR_OP) `(,x))
         (tree (node 'LOGICAL_AND_OP) `(,x))
         (tree (node 'BITWISE_OR_OP) `(,x))
         (tree (node 'BITWISE_AND_OP) `(,x))
         (tree (node 'EQUALITY_OP) `(,x))
         (tree (node 'RELATIONAL_OP) `(,x))
         (tree (node 'ADDITIVE_OP) `(,x))
         (tree (node 'MULTIPLICATIVE_OP) `(,x))
         (tree (node 'UNARY_MINUS_OP) `(,x))
         (tree (node 'UNARY_NOT_OP) `(,x))
         (tree (node 'POSTFIX_EXPRESSION) `(,x))
         (tree (node 'PRIMARY) `(,x))
         (tree (node 'PRIMARY_NO_NEW_ARRAY) `(,x))
         (tree (node 'LHS) `(,x))
         (tree (node 'LITERAL) `(,x))
         (tree (node 'EXPRESSION_OPT) `(,x))) (parse->ast x)]

    [(or (tree (node 'LOGICAL_OR) `(,x ,y ,z))
         (tree (node 'LOGICAL_AND) `(,x ,y ,z))
         (tree (node 'BITWISE_OR) `(,x ,y ,z))
         (tree (node 'BITWISE_AND) `(,x ,y ,z))
         (tree (node 'EQUALITY) `(,x ,y ,z))
         (tree (node 'RELATIONAL) `(,x ,y ,z))
         (tree (node 'ADDITIVE) `(,x ,y ,z))
         (tree (node 'MULTIPLICATIVE) `(,x ,y ,z))) (binop (parse->ast y) (parse->ast x) (parse->ast z))] 

    [(or (tree (node 'UNARY_MINUS) `(,x ,y))
         (tree (node 'UNARY_NOT) `(,x ,y))) (unop (parse->ast y) (parse->ast x))]

    [(tree (node 'CAST) `(,_ ,c ,_ ,expr)) (cast (parse->ast c) (parse->ast expr))]

    [(tree (node 'ARRAY_CREATION_EXPRESSION) `(,_ ,type ,_ ,size ,_)) (arraycreate (parse->ast type) (parse->ast size))]
    [(tree (node 'CLASS_CREATION_EXPRESSION) `(,_ ,class ,_ ,params ,_)) (classcreate (parse->ast class) (parse->ast params))]

    [(tree (node 'FIELD_ACCESS) `(,left ,_ ,field)) (fieldaccess (parse->ast left) (parse->ast field))]
    [(tree (node 'METHOD_CALL) `(,left ,_ ,args ,_)) (methodcall (parse->ast left) (parse->ast args))]

    [(tree (node 'ARGUMENT_LIST) `(,args ,_ ,arg)) (append (parse->ast args) (parse->ast arg))]
    [(tree (node 'ARGUMENT_LIST) `(,arg)) (list (parse->ast arg))]

    [(tree (node 'ARRAY_ACCESS) `(,left ,_ ,index ,_)) (arrayaccess (parse->ast left) (parse->ast index))]

    [(tree (node 'ASSIGNMENT) `(,lhs ,_ ,rhs)) (varassign (parse->ast lhs) (parse->ast rhs))]

    [(tree (node 'SCOPE) `(,x)) (parse->ast x)]

    [(tree (node 'BLOCK_STATEMENTS_OPT) `(,x)) (parse->ast x)]
    [(tree (node 'BLOCK_STATEMENTS) `(,bss ,bs)) (append (parse->ast bss) (parse->ast bs))]
    [(tree (node 'BLOCK_STATEMENTS) `(,bs)) (parse->ast bs)]
    [(tree (node 'BLOCK_STATEMENT) `(,x ,_)) (list (parse->ast x))]
    [(tree (node 'BLOCK_STATEMENT) `(,x)) (list (parse->ast x))]
    [(tree (node 'LOCAL_VARAIABLE_DECLARATION) `(,type ,v)) (var empty empty (parse->ast type) (parse->ast v))]

    [(or (tree (node 'STATEMENT) `(,x))
         (tree (node 'STATEMENT_NO_IF) `(,x))
         (tree (node 'STATEMENT_WITH_NO_SUB_STATEMENT) `(,x))) (parse->ast x)]

    [(tree (node 'IF_STATEMENT) `(,_ ,_ ,test ,_ ,tru)) (iff (parse->ast test) (parse->ast tru) empty)]
    [(or (tree (node 'IF_ELSE_STATEMENT) `(,_ ,_ ,test ,_ ,tru ,_ ,fls))
         (tree (node 'IF_ELSE_STATEMENT_NO_IF) `(,_ ,_ ,test ,_ ,tru ,_ ,fls))) (iff (parse->ast test) (parse->ast tru) (parse->ast fls))]

    [(or (tree (node 'WHILE_STATEMENT) `(,_ ,_ ,test ,_ ,body))
         (tree (node 'WHILE_STATEMENT_NO_IF) `(,_ ,_ ,test ,_ ,body))) (while (parse->ast test) (parse->ast body))]

    [(or (tree (node 'FOR_STATEMENT) `(,_ ,_ ,init ,_ ,clause ,_ ,update ,_ ,body))
         (tree (node 'FOR_STATEMENT_NO_IF) `(,_ ,_ ,init ,_ ,clause ,_ ,update ,_ ,body))) (for (parse->ast init) (parse->ast clause) (parse->ast update) (parse->ast body))]

    [(tree (node 'FOR_INIT) `(,x)) (parse->ast x)]
    [(tree (node 'FOR_UPDATE) `(,x)) (parse->ast x)]

    [(tree (node 'EXPRESSION_STATEMENT) `(,x ,_)) (parse->ast x)]
    [(tree (node 'STATEMENT_EXPRESSION) `(,x)) (parse->ast x)]
    [(tree (node 'RETURN_STATEMENT) `(,_ ,x ,_)) (return (parse->ast x))]

    [(tree (leafnode (token 'void x)) _) 'void]
    [(or (tree (leafnode (token 'id x)) _) 
         (tree (leafnode (token 'decimal-lit x)) _) 
         (tree (leafnode (token 'null-lit x)) _)
         (tree (leafnode (token 'string-lit x)) _)
         (tree (leafnode (token 'char-lit x)) _)
         (tree (leafnode (token 'bool-lit x)) _)) x]

    [(tree (leafnode (token 'semi x)) _) empty]
    [(tree (leafnode (token x _)) _) x]
))






  












