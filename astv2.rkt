#lang racket

(require "scanner.rkt")
(require "parse-tree.rkt")

(provide (struct-out tree))
(provide (struct-out node))
(provide (struct-out leafnode))
(provide (struct-out cunit))
(provide (struct-out cimport))
(provide (struct-out pimport))
(provide (struct-out interface))
(provide (struct-out class))
(provide (struct-out constructor))
(provide (struct-out method))
(provide (struct-out methoddecl))
(provide (struct-out parameter))
(provide (struct-out var))
(provide (struct-out varassign))
(provide (struct-out binop))
(provide (struct-out unop))
(provide (struct-out cast))
(provide (struct-out arraycreate))
(provide (struct-out classcreate))
(provide (struct-out fieldaccess))
(provide (struct-out methodcall))
(provide (struct-out arrayaccess))
(provide (struct-out iff))
(provide (struct-out while))
(provide (struct-out for))
(provide (struct-out return))
(provide (struct-out ptype))
(provide (struct-out rtype))
(provide (struct-out atype))
(provide (struct-out block))

(provide parse->ast)
(provide print-asts)

;==============================================================================================
;==== AST Structures
;==============================================================================================

;(struct node ([id : Symbol] [astnode : "any other struct below"]))
(struct astnode (id astnode) #:transparent)

;(struct c-unit ([package : (Listof String)] [imports : (Listof (Listof String))] [body : class U import]))
(struct cunit (package imports body) #:transparent)

;(struct cimport ([path : (Listof String)]))
(struct cimport (path) #:transparent)

;(struct pimport ([path : (Listof String)]))
(struct pimport (path) #:transparent)

;(struct interface ([scope : Symbol] [mod : Symbol] [id : String] [extends : (Listof String)] [body : block]))
(struct interface (scope mod id extends body) #:transparent)

;(struct class ([scope : Symbol] [mod : Symbol] [id : String] [extends : (Listof String)] [extends : (Listof (Listof String))] [body : block]))
(struct class (scope mod id extends implements body) #:transparent)

;(struct constructor ([scope : Symbol] [methoddecl : methoddecl] [body : block]))
(struct constructor (scope methoddecl body) #:transparent)

;(struct method ([scope : Symbol] [mod : (Listof Symbol)] [methoddecl : methoddecl] [body : block]))
(struct method (scope mod type methoddecl body) #:transparent)

;(struct methoddecl ([id : String] [parameters : (Listof parameter)]))
(struct methoddecl (id parameters) #:transparent)

;(struct parameter ([type : (ptype, rtype, atype)] [id : String]))
(struct parameter (type id) #:transparent)

;(struct var ([scope : Symbol] [mod : Symbol] [type : (ptype, rtype, atype)] [var-assign : varassign]))
(struct var (scope mod type var-assign) #:transparent)

;(struct varassign ([id : String] [expr : "alot of things"]))
(struct varassign (id expr) #:transparent)

;(struct binop ([op : Symbol] [left: "expression"] [right: "expression"]))
(struct binop (op left right) #:transparent)

;(struct unop ([op : Symbol] [right: "expression"]))
(struct unop (op right) #:transparent)

;(struct cast ([c : Symbol] [expr: "expression"]))
(struct cast (c expr) #:transparent)

;(struct arraycreate ([type : (ptype, (Listof String))] [size: "expression"]))
(struct arraycreate (type size) #:transparent)

;(struct classcreate ([class : (Listof String)] [params: (Listof "expression")]))
(struct classcreate (class params) #:transparent)

;(struct fieldaccess ([left : "primary"] [field: String]))
(struct fieldaccess (left field) #:transparent)

;(struct methodcall ([left : (Listof String) | fieldaccess] [args: (Listof "expression")]))
(struct methodcall (left args) #:transparent)

;(struct arrayaccess ([left : (Listof String) | "primary, no new arrays"] [index: "expression"]))
(struct arrayaccess (left index) #:transparent)

;(struct iff ([test : "expression"] [tru: block | "lots of things"] [fls: block | "lots of things"]))
(struct iff (test tru fls) #:transparent)

;(struct while ([test : "expression"] [body: block | "lots of things"]))
(struct while (test body) #:transparent)

;(struct for ([init : var | varassign | methodcall | classcreate] [clause: "expression"] [update: varassign | methodcall | classcreate]))
(struct for (init clause update body) #:transparent)

;(struct return ([expr : "expression"]))
(struct return (expr) #:transparent)

;(struct ptype ([type : Symbol]))
(struct ptype (type) #:transparent)

;(struct rtype ([type : Symbol]))
(struct rtype (type) #:transparent)

;(struct atype ([type : Symbol]))
(struct atype (type) #:transparent)

;(struct block ([statements : (Listof "lots of things")]))
(struct block (statements) #:transparent)

;==============================================================================================
;==== AST Generation
;==============================================================================================

(define (parse->ast t)
  (match t
    [(tree (node _) '()) empty]
    
;==============================================================================================
;==== AST Structure Creating Rules
;==============================================================================================

    ;cunit
    [(tree (node 'S) `(,x ,y ,z)) (astnode (gensym) (cunit (parse->ast x) (parse->ast y) (parse->ast z)))]
    
    ;cimport/pimport
    [(tree (node 'CLASS_IMPORT) `(,_ ,x ,_)) (astnode (gensym) (cimport (parse->ast x)))]
    [(tree (node 'PACKAGE_IMPORT) `(,_ ,x ,_ ,_ ,_)) (astnode (gensym) (pimport (parse->ast x)))]
    
    ;interface
    [(tree (node 'INTERFACE_DECLARATION) x) (astnode (gensym) (interface (parse->ast (list-ref x 0)) (parse->ast (list-ref x 1)) (parse->ast (list-ref x 3)) (parse->ast (list-ref x 4)) (astnode (gensym) (block (parse->ast (list-ref x 5))))))]
    
    ;class
    [(tree (node 'CLASS_DECLARATION) x) (astnode (gensym) (class (parse->ast (list-ref x 0)) (parse->ast (list-ref x 1)) (parse->ast (list-ref x 3)) (parse->ast (list-ref x 4)) (parse->ast (list-ref x 5)) (astnode (gensym) (block (parse->ast (list-ref x 6))))))]
    
    ;constructor
    [(tree (node 'CONSTRUCTOR_DECLARATION) `(,scope ,decl ,body)) (astnode (gensym) (constructor (parse->ast scope) (parse->ast decl) (parse->ast body)))]
    
    ;method
    [(tree (node 'NORMAL_METHOD_DECLARATION) `(,scope ,type ,decl ,body)) (astnode (gensym) (method (parse->ast scope) empty (parse->ast type) (parse->ast decl) (parse->ast body)))]
    [(tree (node 'ABSTRACT_METHOD_DECLARATION) `(,scope ,mod ,type ,decl ,body)) (astnode (gensym) (method (parse->ast scope) (list (parse->ast mod)) (parse->ast type) (parse->ast decl) (parse->ast body)))]
    [(tree (node 'FINAL_METHOD_DECLARATION) `(,scope ,mod ,type ,decl ,body)) (astnode (gensym) (method (parse->ast scope) (list (parse->ast mod)) (parse->ast type) (parse->ast decl) (parse->ast body)))]
    [(tree (node 'STATIC_METHOD_DECLARATION) `(,scope ,mod ,type ,decl ,body)) (astnode (gensym) (method (parse->ast scope) (list (parse->ast mod)) (parse->ast type) (parse->ast decl) (parse->ast body)))]
    [(tree (node 'STATIC_NATIVE_METHOD_DECLARATION) `(,scope ,mod ,type ,decl ,body)) (astnode (gensym) (method (parse->ast scope) (parse->ast mod) (ptype (parse->ast type)) (parse->ast decl) (parse->ast body)))]
    [(tree (node 'NORMAL_METHOD_DECLARATION_NO_BODY) `(,scope ,type ,decl ,body)) (astnode (gensym) (method (parse->ast scope) empty (parse->ast type) (parse->ast decl) (parse->ast body)))]

    ;methoddecl
    [(tree (node 'METHOD_DECLARATOR) `(,id ,_ ,params ,_)) (astnode (gensym) (methoddecl (parse->ast id) (parse->ast params)))]
    [(tree (node 'STATIC_NATIVE_BODY) `(,id ,_ ,int ,p-id ,_)) (astnode (gensym) (methoddecl (parse->ast id) (list (parameter (parse->ast int) (parse->ast p-id)))))]

    ;return
    [(tree (node 'RETURN_STATEMENT) `(,_ ,x ,_)) (astnode (gensym) (return (parse->ast x)))]

    ;ptype/rtype/atype
    [(tree (node 'PRIMITIVE_TYPE) `(,x)) (astnode (gensym) (ptype (parse->ast x)))]
    [(tree (node 'REFERENCE_TYPE) `(,x)) (astnode (gensym) (rtype (parse->ast x)))]
    [(tree (node 'ARRAY_TYPE) x) (astnode (gensym) (atype (parse->ast (first x))))]

    ;var
    [(tree (node 'LOCAL_VARAIABLE_DECLARATION) `(,type ,v)) (astnode (gensym) (var empty empty (parse->ast type) (parse->ast v)))]
    [(tree (node 'CLASS_VARIABLE_DECLARATION) `( ,x ,type ,v ,_ )) (astnode (gensym) (var (parse->ast x) empty (parse->ast type) (parse->ast v)))]
    [(tree (node 'STATIC_CLASS_VARIABLE_DECLARATION) `( ,x ,_ ,type ,v ,_ )) (astnode (gensym) (var (parse->ast x) 'static (parse->ast type) (parse->ast v)))]

    ;varassign
    [(tree (node 'VARIABLE_DECLARATOR) `( ,id ,_ ,expr )) (astnode (gensym) (varassign (parse->ast id) (parse->ast expr))) ]
    [(tree (node 'ASSIGNMENT) `(,lhs ,_ ,rhs)) (astnode (gensym) (varassign (parse->ast lhs) (parse->ast rhs)))]

    ;binop
    [(or (tree (node 'LOGICAL_OR) `(,x ,y ,z))
         (tree (node 'LOGICAL_AND) `(,x ,y ,z))
         (tree (node 'BITWISE_OR) `(,x ,y ,z))
         (tree (node 'BITWISE_AND) `(,x ,y ,z))
         (tree (node 'EQUALITY) `(,x ,y ,z))
         (tree (node 'RELATIONAL) `(,x ,y ,z))
         (tree (node 'ADDITIVE) `(,x ,y ,z))
         (tree (node 'MULTIPLICATIVE) `(,x ,y ,z))) (astnode (gensym) (binop (parse->ast y) (parse->ast x) (parse->ast z)))] 

    ;unop
    [(or (tree (node 'UNARY_MINUS) `(,x ,y))
         (tree (node 'UNARY_NOT) `(,x ,y))) (astnode (gensym) (unop (parse->ast y) (parse->ast x)))]

    ;block
    [(tree (node 'BLOCK) `(,_ ,x ,_)) (astnode (gensym) (block (parse->ast x)))]

    ;cast
    [(tree (node 'CAST) `(,_ ,c ,_ ,expr)) (astnode (gensym) (cast (parse->ast c) (parse->ast expr)))]

    ;arraycreate
    [(tree (node 'ARRAY_CREATION_EXPRESSION) `(,_ ,type ,_ ,size ,_)) (astnode (gensym) (arraycreate (parse->ast type) (parse->ast size)))]

    ;classcreate
    [(tree (node 'CLASS_CREATION_EXPRESSION) `(,_ ,class ,_ ,params ,_)) (astnode (gensym) (classcreate (parse->ast class) (parse->ast params)))]

    ;fieldaccess
    [(tree (node 'FIELD_ACCESS) `(,left ,_ ,field)) (astnode (gensym) (fieldaccess (parse->ast left) (parse->ast field)))]

    ;methodcall
    [(tree (node 'METHOD_CALL) `(,left ,_ ,args ,_)) (astnode (gensym) (methodcall (parse->ast left) (parse->ast args)))]

    ;arrayaccess
    [(tree (node 'ARRAY_ACCESS) `(,left ,_ ,index ,_)) (astnode (gensym) (arrayaccess (parse->ast left) (parse->ast index)))]

    ;iff
    [(tree (node 'IF_STATEMENT) `(,_ ,_ ,test ,_ ,tru)) (astnode (gensym) (iff (parse->ast test) (parse->ast tru) empty))]
    [(tree (node 'IF_ELSE_STATEMENT) `(,_ ,_ ,test ,_ ,tru ,_ ,fls)) (astnode (gensym) (iff (parse->ast test) (parse->ast tru) (parse->ast fls)))]
    [(tree (node 'IF_ELSE_STATEMENT_NO_IF) `(,_ ,_ ,test ,_ ,tru ,_ ,fls)) (astnode (gensym) (iff (parse->ast test) (parse->ast tru) (parse->ast fls)))]

    ;while
    [(tree (node 'WHILE_STATEMENT) `(,_ ,_ ,test ,_ ,body)) (astnode (gensym) (while (parse->ast test) (parse->ast body)))]
    [(tree (node 'WHILE_STATEMENT_NO_IF) `(,_ ,_ ,test ,_ ,body)) (astnode (gensym) (while (parse->ast test) (parse->ast body)))]

    ;for
    [(tree (node 'FOR_STATEMENT) `(,_ ,_ ,init ,_ ,clause ,_ ,update ,_ ,body)) (astnode (gensym) (for (parse->ast init) (parse->ast clause) (parse->ast update) (parse->ast body)))]
    [(tree (node 'FOR_STATEMENT_NO_IF) `(,_ ,_ ,init ,_ ,clause ,_ ,update ,_ ,body)) (astnode (gensym) (for (parse->ast init) (parse->ast clause) (parse->ast update) (parse->ast body)))]
    
;==============================================================================================
;==== REDUCTION RULES, all rules below serve to strip junk from the parse tree
;==============================================================================================

    [(tree (node 'PACKAGE) `(,_ ,x ,_)) (parse->ast x)]
    [(tree (node 'IMPORTS) `(,x)) (parse->ast x)]
    [(tree (node 'IMPORT_LIST) `(,x ,y)) (append (parse->ast x) (list (parse->ast y)))]
    [(tree (node 'IMPORT_LIST) `(,x)) (list (parse->ast x))]
    [(tree (node 'IMPORT) `(,x)) (parse->ast x)]

    [(tree (node 'CLASS_OR_INTERFACE_DECLARATION) `(,x)) (parse->ast x)]

    [(tree (node 'EXTENDS) `(,x ,y)) (parse->ast y)]
    [(tree (node 'IMPLEMENTS) `(,x ,y)) (parse->ast y)]

    [(tree (node 'IMPLEMENTS_TYPE_LIST) `(,x ,_ ,y)) (append (parse->ast x) (list (parse->ast y)))]
    [(tree (node 'IMPLEMENTS_TYPE_LIST) `(,x)) (list (parse->ast x))]

    [(tree (node 'INTERFACE_BODY_DECLARATIONS_OPT) `(,x)) (parse->ast x)]
    [(tree (node 'INTERFACE_BODY_DECLARATIONS) `(,x ,y)) (append (parse->ast x) (parse->ast y))]
    [(tree (node 'INTERFACE_BODY_DECLARATIONS) `(,x)) (parse->ast x)]
    [(tree (node 'INTERFACE_BODY_DECLARATION) `(,x)) (list (parse->ast x))]

    [(tree (node 'CLASS_BODY_DECLARATIONS) `(,x ,y)) (append (parse->ast x) (parse->ast y))]
    [(tree (node 'CLASS_BODY_DECLARATIONS) `(,x)) (parse->ast x)]
    [(tree (node 'CLASS_BODY_DECLARATION) `(,x)) (list (parse->ast x))]

    [(tree (node 'STATIC_NATIVE) `(,x ,y)) (list (parse->ast x) (parse->ast x))]

    [(tree (node 'PARAMETER_LIST) `(,params ,_ ,param)) (append (parse->ast params) (parse->ast param))]
    [(tree (node 'PARAMETER_LIST) `(,param)) (parse->ast param)]
    [(tree (node 'PARAMETER) `(,type ,id)) (list (parameter (parse->ast type) (parse->ast id)))]

    [(tree (node 'INTERFACE_MODIFIER) `(,x)) (parse->ast x)]
    [(tree (node 'CLASS_MODIFIER) `(,x)) (parse->ast x)]

    [(tree (node 'TYPE) `(,x)) (parse->ast x)]

    [(tree (node 'VARIABLE_DECLARATOR_OPT) `( ,x )) (parse->ast x) ]

    [(tree (node 'IDS) `(,id1 ,dot ,id2)) (append (parse->ast id1) (list (parse->ast id2)))]
    [(tree (node 'IDS) `(,id)) (list (parse->ast id))]

    [(or (tree (node 'PRIMARY_NO_NEW_ARRAY) `(,_ ,x ,_))
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

    [(tree (node 'ARGUMENT_LIST) `(,args ,_ ,arg)) (append (parse->ast args) (parse->ast arg))]
    [(tree (node 'ARGUMENT_LIST) `(,arg)) (list (parse->ast arg))]

    [(tree (node 'SCOPE) `(,x)) (parse->ast x)]

    [(tree (node 'BLOCK_STATEMENTS_OPT) `(,x)) (parse->ast x)]
    [(tree (node 'BLOCK_STATEMENTS) `(,bss ,bs)) (append (parse->ast bss) (parse->ast bs))]
    [(tree (node 'BLOCK_STATEMENTS) `(,bs)) (parse->ast bs)]
    [(tree (node 'BLOCK_STATEMENT) `(,x ,_)) (list (parse->ast x))]
    [(tree (node 'BLOCK_STATEMENT) `(,x)) (list (parse->ast x))]

    [(or (tree (node 'STATEMENT) `(,x))
         (tree (node 'STATEMENT_NO_IF) `(,x))
         (tree (node 'STATEMENT_WITH_NO_SUB_STATEMENT) `(,x))) (parse->ast x)]

    [(tree (node 'FOR_INIT) `(,x)) (parse->ast x)]
    [(tree (node 'FOR_UPDATE) `(,x)) (parse->ast x)]

    [(tree (node 'EXPRESSION_STATEMENT) `(,x ,_)) (parse->ast x)]
    [(tree (node 'STATEMENT_EXPRESSION) `(,x)) (parse->ast x)]

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

;==============================================================================================
;==== Print
;==============================================================================================

(define (print-asts asts files)
  (for-each (lambda(ast) (printf "~n================ AST ================~n") (print-ast ast "" "")) asts))

(define (print-ast ast-node indent suffix)
  (match ast-node
    [(astnode id astnode) ;(printf "~a#[~a " indent id)
                          (printf "~a#[" indent)
		          (print-ast astnode indent suffix)]

    [(cunit package imports body) (printf "c-unit~n  ~a~n" package)
			          (for-each (lambda (x) (print-ast x (string-append indent "  ") "\n")) imports)
			          (print-ast body (string-append indent "  ") "")]

    [(interface scope mod id extends body) (printf "interface") 
			                   (print-ast scope " " "")
			                   (print-ast mod " " "")
			                   (print-ast id " " "")
			                   (print-ast extends " " "\n")
			                   (print-ast body (string-append indent "  ") "]\n")]

    [(class scope mod id extends implements body) (printf "class")
			                          (print-ast scope " " "")
			                          (print-ast mod " " "")
			                          (print-ast id " " "")
			                          (print-ast extends " " "")
			                          (print-ast implements " " "\n")
			                          (print-ast body (string-append indent "  ") "]\n")]

    [(constructor scope methoddecl body) (printf "constructor")
			                  (print-ast scope " " "")
			                  (print-ast methoddecl " " "\n")
			                  (print-ast body (string-append indent "  ") "]\n")]

    [(method scope mod type methoddecl body) (printf "method")
			                      (print-ast scope " " "")
			                      (print-ast mod " " "")
			                      (print-ast type " " "")
			                      (print-ast methoddecl " " "\n")
			                      (print-ast body (string-append indent "  ") "]\n")]

    [(iff test tru fls) (printf "iff")
		        (print-ast test " " "\n")
		        (print-ast tru (string-append indent "  ") "\n")
		        (print-ast fls (string-append indent "  ") "]\n")]

    [(while test body) (printf "while") 
		       (print-ast test  " " "\n")
	       	       (print-ast body (string-append indent "  ") "]\n")]

    [(for init clause update body) (printf "for") 
		                   (print-ast init " " "")
		                   (print-ast clause " " "")
		                   (print-ast update " " "\n")
		                   (print-ast body (string-append indent "  ") "]\n")]

    [(block statements) (printf "block~n") 
                           (for-each (lambda (x) (print-ast x (string-append indent "|  ") "\n")) statements)]

    [(return expr) (printf "return") (print-ast expr " " (string-append "]" suffix))]
    [(ptype type) (printf "ptype") (print-ast type " " (string-append "]" suffix))]
    [(rtype type) (printf "rtype") (print-ast type " " (string-append "]" suffix))]
    [(atype type) (printf "atype") (print-ast type " " (string-append "]" suffix))]

    [(methoddecl id parameters) (printf "methoddecl")
                                (print-ast id " " "")
                                (printf " (")
			        (for-each (lambda (x) (print-ast x " " "")) parameters)
                                (printf " )~a" suffix)]

    [(parameter type id) (printf "parameter")
                          (print-ast type " " "")
                          (print-ast id " " (string-append "]" suffix))]

    [(var scope mod type var-assign) (printf "var")
                                      (print-ast scope " " "")
                                      (print-ast mod " " "")
                                      (print-ast type " " "")
                                      (print-ast var-assign " " (string-append "]" suffix))]

    [(varassign id expr) (printf "varassign")
                                      (print-ast id " " "")
                                      (print-ast expr " " (string-append "]" suffix))]

    [(binop op left right) (printf "binop")
                                      (print-ast op " " "")
                                      (print-ast left " " "")
                                      (print-ast right " " (string-append "]" suffix))]

    [(unop op right) (printf "unop")
                                      (print-ast op " " "")
                                      (print-ast right " " (string-append "]" suffix))]

    [(cast c expr) (printf "cast")
                                      (print-ast c " " "")
                                      (print-ast expr " " (string-append "]" suffix))]

    [(arraycreate type size) (printf "arraycreate")
                                      (print-ast type " " "")
                                      (print-ast size " " (string-append "]" suffix))]

    [(classcreate class params) (printf "classcreate")
                                      (print-ast class " " "")
                                      (print-ast params " " (string-append "]" suffix))]

    [(fieldaccess left field) (printf "fieldaccess")
                                      (print-ast left " " "")
                                      (print-ast field " " (string-append "]" suffix))]

    [(methodcall left args) (printf "methodcall")
                                      (print-ast left " " "")
                                      (print-ast args " " (string-append "]" suffix))]

    [(arrayaccess left index) (printf "arrayaccess")
                                      (print-ast left " " "")
                                      (print-ast index " " (string-append "]" suffix))]

    [_ (printf "~a~a" indent ast-node) (display suffix)]))






  












