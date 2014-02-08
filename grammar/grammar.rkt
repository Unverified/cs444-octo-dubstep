#lang racket

(require "../token.rkt")

(struct rule (lhs rhs))
(provide operator-rules)

(define start 'Sp)
(define terminals (append (list 'BOF 'EOF) (remove-duplicates (map first token-exps))))
(define non-terminals (list 'Sp 'S 'CLASS 'DECLS 'DECL 'TYPE 'ARRAY_TYPE 'IDS 'SCOPE 'ARGS 'ARG_TYPE_LIST 'ARG 'LITERAL 'CMOD 'CONSTRUCTOR 'NORMAL_FUNC 'FINAL_FUNC 'STATIC_FUNC 'ABS_FUNC 'FUNC_BODY 'ABS_BODY 'MEMBER_VAR 'STATIC_MEMBER_VAR 'MEMBER_VAR_DEF 'NATIVE_FUNC 'NATIVE_BODY 'STATIC_NATIVE 'STATEMENTS 'STATEMENT 'BLOCK 'BLOCK_BODY 'IF 'FOR 'FOR_PARAMS 'FOR_ASSIGN 'FOR_CLAUSE 'FOR_ITER 'WHILE 'STATEMENT_BODY 'ELSE_CLAUSES 'ELSE_IF 'ELSE 'COND_ARITH_EXP 'ARITH_EXP 'ARITH_TERM 'ARITH_FACTOR 'ARITH_EXP2 'ARITH_TERM2 'ARITH_FACTOR2 'ASSIGNMENT_OP 'LOGICAL_OR_OP 'LOGICAL_AND_OP 'BITWISE_OR_OP 'BITWISE_AND_OP 'EQUALITY_OP 'RELATIONAL_OP 'MULT_OP 'ADDITIVE_OP 'CP5 'CP4 'CP3 'CP2 'CP1 'CP0 'EXPRESSION 'VAR_DEFINITION 'VAR_ASSIGNMENT 'FUNCTION_CALL 'FUNCTION_ARGS 'FUNC_ARG_TYPE_LIST 'RETURN 'CLASS_ADDITION 'PACKAGE 'PACKAGE_IMPORT 'IMPORTS 'IMPORT 'CALL_CHANG 'FUNCTION_CALLS 'FUNCTION_CALL_EXP 'CLASS_IMPORT 'FIELD_ACCESS 'ID_FUNCTION_DOT_LIST 'PRIMITIVE 'NEW 'ARG_LIST 'FUNC_ARG_CALL 'FUNC_ARG_DEF 'SOMETHING 'ARGUMENTS 'THIS_ACCESS 'NEW_OBJECT 'NEW_ARRAY 'CAST 'ARITH_STATEMENT 'CAST_OBJ 'CP6 'ENCLOSED_IDS  'ENCLOSED_PRIMITIVE 'CP2b 'DECL_NO_BODY 'DECLS_NO_BODY 'INTERFACE 'IMOD 'INTERFACE_ADDITION 'JCLASS 'IMPLEMENTS 'CASTS 'THROWS))

;==============================================================================================
;==== Literals
;==============================================================================================

(define literal-rules
  (list  
    (rule 'LITERAL (list 'decimal-lit))
    (rule 'LITERAL (list 'null-lit))
    (rule 'LITERAL (list 'bool-lit))))

;==============================================================================================
;==== Operator Rules (Broken up in precedence level with high precedence at top)
;==============================================================================================

(define multiplicative-op-rules
  (list
    (rule 'MULT_OP (list 'star))
    (rule 'MULT_OP (list 'slash))
    (rule 'MULT_OP (list 'pct))))

(define additive-op-rules
  (list
    (rule 'ADDITIVE_OP (list 'plus))
    (rule 'ADDITIVE_OP (list 'minus))))

(define relational-op-rules
  (list
    (rule 'RELATIONAL_OP (list 'lt))
    (rule 'RELATIONAL_OP (list 'gt))
    (rule 'RELATIONAL_OP (list 'lteq))
    (rule 'RELATIONAL_OP (list 'gteq))))

(define equality-op-rules
  (list
    (rule 'EQUALITY_OP (list 'eqeq))
    (rule 'EQUALITY_OP (list 'noteq))))

(define bitwise-and-op-rules
  (list
    (rule 'BITWISE_AND_OP (list 'amp))))

(define bitwise-or-op-rules
  (list
    (rule 'BITWISE_OR_OP (list 'bar))))

(define logical-and-op-rules
  (list
    (rule 'LOGICAL_AND_OP (list 'ampamp))))

(define logical-or-op-rules
  (list
    (rule 'LOGICAL_OR_OP (list 'barbar))))

(define assignment-op-rules
  (list
    (rule 'ASSIGNMENT_OP (list 'eq))))

;==============================================================================================
;==== Expressions
;==============================================================================================

(define condition-expression-rules
  (list
    (rule 'COND_ARITH_EXP (list 'CASTS 'CP6))    ; int x = (id)-1
						
    (rule 'COND_ARITH_EXP (list 'CP6))
    (rule 'CP6 (list 'CP6 'LOGICAL_OR_OP 'CP5))
    (rule 'CP6 (list 'CP5))
    (rule 'CP5 (list 'CP5 'LOGICAL_AND_OP 'CP4))
    (rule 'CP5 (list 'CP4))
    (rule 'CP4 (list 'CP4 'BITWISE_OR_OP 'CP3))
    (rule 'CP4 (list 'CP3))
    (rule 'CP3 (list 'CP3 'BITWISE_AND_OP 'CP2))
    (rule 'CP3 (list 'CP2))
    (rule 'CP2 (list 'CP2 'EQUALITY_OP 'CP2b))
    (rule 'CP2 (list 'CP2b))
    (rule 'CP2b (list 'CP2b 'instanceof 'TYPE))
    (rule 'CP2b (list 'CP1))
    (rule 'CP1 (list 'CP1 'RELATIONAL_OP 'CP0))
    (rule 'CP1 (list 'CP0))
    (rule 'CP0 (list 'not 'ARITH_EXP))
    (rule 'CP0 (list 'ARITH_EXP))
))

; Yes, ARITH_EXP is purposely looped back to COND_EXP, this is so COND_EXP can have ARITH_EXP. ARITH_EXP
; needs to be appended on the end of COND_EXP because of precedence (ie need to do arithmitic before conditions
; are checked
(define arithmetic-expression-rules
  (list
    (rule 'ARITH_EXP (list 'ARITH_EXP 'ADDITIVE_OP 'ARITH_TERM))
    (rule 'ARITH_EXP (list 'ARITH_TERM))
    (rule 'ARITH_TERM (list 'ARITH_TERM 'MULT_OP 'ARITH_FACTOR))
    (rule 'ARITH_TERM (list 'ARITH_FACTOR))
    (rule 'ARITH_FACTOR (list 'oparen 'minus 'COND_ARITH_EXP 'cparen))
    (rule 'ARITH_FACTOR (list 'oparen 'minus 'COND_ARITH_EXP 'cparen 'osquare 'COND_ARITH_EXP 'csquare))
    (rule 'ARITH_FACTOR (list 'oparen 'COND_ARITH_EXP 'cparen))
    (rule 'ARITH_FACTOR (list 'oparen 'COND_ARITH_EXP 'cparen 'osquare 'COND_ARITH_EXP 'csquare))
    (rule 'ARITH_FACTOR (list 'IDS))
    (rule 'ARITH_FACTOR (list 'LITERAL))
    (rule 'ARITH_FACTOR (list 'FUNCTION_CALL_EXP))	
    (rule 'ARITH_FACTOR (list 'THIS_ACCESS))	
    (rule 'ARITH_FACTOR (list 'NEW))  
))

(define primitive-rules
  (list
    (rule 'PRIMITIVE (list 'boolean))
    (rule 'PRIMITIVE (list 'int))
    (rule 'PRIMITIVE (list 'char))
    (rule 'PRIMITIVE (list 'byte))
    (rule 'PRIMITIVE (list 'short))))


(define type-rules
  (list
    (rule 'TYPE (list 'PRIMITIVE))
    (rule 'TYPE (list 'IDS))
    (rule 'TYPE (list 'ARRAY_TYPE))
    (rule 'TYPE (list 'void))
    (rule 'ARRAY_TYPE (list 'PRIMITIVE 'osquare 'csquare))))

(define class-mod-rules
  (list
    (rule 'CMOD (list 'final))
    (rule 'CMOD (list 'static))
    (rule 'CMOD (list 'abstract))
    (rule 'CMOD empty)
    (rule 'IMOD (list 'abstract))
    (rule 'IMOD empty)))

(define scope-rules
  (list
    (rule 'SCOPE (list 'public))
    (rule 'SCOPE (list 'protected))))

(define other-rules 
   (list 
    ; Start
    (rule 'Sp (list 'BOF 'S 'EOF))
    (rule 'S (list 'PACKAGE 'IMPORTS 'JCLASS))

    ; CLASS
    (rule 'PACKAGE (list 'package 'IDS 'semi))
    (rule 'PACKAGE empty)

    ; CLASS
    (rule 'IMPORTS (list 'IMPORTS 'IMPORT))
    (rule 'IMPORTS empty)
    (rule 'IMPORT (list 'import 'PACKAGE_IMPORT 'semi))
    (rule 'IMPORT (list 'import 'CLASS_IMPORT 'semi))
    (rule 'PACKAGE_IMPORT (list 'CLASS_IMPORT 'dot 'star))
    (rule 'PACKAGE_IMPORT (list 'star))
    (rule 'CLASS_IMPORT (list 'CLASS_IMPORT 'dot 'id))
    (rule 'CLASS_IMPORT (list 'id))

    (rule 'JCLASS (list 'CLASS))
    (rule 'JCLASS (list 'INTERFACE))

    ; CLASS
    (rule 'CLASS (list 'public 'CMOD 'class 'id 'CLASS_ADDITION 'ocurl 'DECLS 'ccurl))
    (rule 'INTERFACE (list 'public 'IMOD 'interface 'id 'INTERFACE_ADDITION 'ocurl 'DECLS_NO_BODY 'ccurl))

    ; DECLS
    (rule 'DECLS_NO_BODY (list 'DECLS_NO_BODY 'DECL_NO_BODY))
    (rule 'DECLS_NO_BODY empty)

    ; DECL
    (rule 'DECL_NO_BODY (list 'SCOPE 'TYPE 'id 'oparen 'ARGUMENTS 'cparen 'semi))
    (rule 'DECL_NO_BODY (list 'SCOPE 'abstract 'TYPE 'id 'oparen 'ARGUMENTS 'cparen 'semi))

    ; INTERFACE_ADDITION
    (rule 'INTERFACE_ADDITION (list 'implements 'IDS))
    (rule 'INTERFACE_ADDITION empty)

    ; CLASS_ADDITION
    (rule 'CLASS_ADDITION (list 'extends 'IDS))
    (rule 'CLASS_ADDITION (list 'implements 'IMPLEMENTS))
    (rule 'CLASS_ADDITION empty)

    (rule 'IMPLEMENTS (list 'IMPLEMENTS 'comma 'IDS))
    (rule 'IMPLEMENTS (list 'IDS ))

    ; DECLS
    (rule 'DECLS (list 'DECLS 'DECL))
    (rule 'DECLS (list 'DECL))

    ; DECL
    (rule 'DECL (list 'CONSTRUCTOR))
    (rule 'DECL (list 'NORMAL_FUNC))
    (rule 'DECL (list 'FINAL_FUNC))
    (rule 'DECL (list 'STATIC_FUNC))
    (rule 'DECL (list 'NATIVE_FUNC))
    (rule 'DECL (list 'ABS_FUNC))
    (rule 'DECL (list 'MEMBER_VAR))
    (rule 'DECL (list 'STATIC_MEMBER_VAR))

    ; Function/Construtor rules
    (rule 'CONSTRUCTOR (list 'SCOPE 'FUNC_BODY))
    (rule 'NORMAL_FUNC (list 'SCOPE 'TYPE 'FUNC_BODY))
    (rule 'FINAL_FUNC (list 'SCOPE 'final 'TYPE 'FUNC_BODY))
    (rule 'STATIC_FUNC (list 'SCOPE 'static 'TYPE 'FUNC_BODY))
    (rule 'NATIVE_FUNC (list 'SCOPE 'STATIC_NATIVE 'int 'NATIVE_BODY))
    (rule 'STATIC_NATIVE (list 'static 'native))
    (rule 'ABS_FUNC (list 'SCOPE 'abstract 'TYPE 'ABS_BODY))
    (rule 'FUNC_BODY (list 'id 'oparen 'ARGUMENTS 'cparen 'THROWS 'BLOCK))
    (rule 'ABS_BODY (list 'id 'oparen 'ARGUMENTS 'cparen 'THROWS 'semi))
    (rule 'NATIVE_BODY (list 'id 'oparen 'int 'id 'cparen 'semi))

    (rule 'THROWS (list 'throw 'IDS))
    (rule 'THROWS empty)

    ; ARGS
    (rule 'ARGS (list 'ARG_TYPE_LIST))
    (rule 'ARGS empty)
    (rule 'ARG_TYPE_LIST (list 'ARG 'comma 'ARG_TYPE_LIST))
    (rule 'ARG_TYPE_LIST (list 'ARG))
    (rule 'ARG (list 'TYPE 'id))

    ; Variable rules
    (rule 'MEMBER_VAR (list 'SCOPE 'MEMBER_VAR_DEF 'semi))
    (rule 'STATIC_MEMBER_VAR (list 'SCOPE 'static 'MEMBER_VAR_DEF 'semi))
    (rule 'MEMBER_VAR (list 'MEMBER_VAR_DEF 'semi))
    (rule 'STATIC_MEMBER_VAR (list 'static 'MEMBER_VAR_DEF 'semi))

    ; Variable assigment
    (rule 'MEMBER_VAR_DEF (list 'TYPE 'id))
    (rule 'MEMBER_VAR_DEF (list 'VAR_DEFINITION))
    (rule 'VAR_DEFINITION (list 'TYPE 'VAR_ASSIGNMENT))
    (rule 'VAR_ASSIGNMENT (list 'id 'eq 'COND_ARITH_EXP))
    (rule 'VAR_ASSIGNMENT (list 'NEW_OBJECT 'eq 'COND_ARITH_EXP))

    ; BLOCK
    (rule 'BLOCK (list 'ocurl 'BLOCK_BODY 'ccurl))
    (rule 'BLOCK_BODY (list 'STATEMENTS))
    (rule 'BLOCK_BODY empty)

    ; STATEMENTS
    (rule 'STATEMENTS (list 'STATEMENTS 'STATEMENT))
    (rule 'STATEMENTS (list 'STATEMENT))

    ; STATEMENT
    (rule 'STATEMENT (list 'VAR_DEFINITION 'semi))
    (rule 'STATEMENT (list 'VAR_ASSIGNMENT 'semi))
    (rule 'STATEMENT (list 'FUNCTION_CALL_EXP 'semi))
    (rule 'STATEMENT (list 'THIS_ACCESS 'semi))
    (rule 'STATEMENT (list 'BLOCK))
    (rule 'STATEMENT (list 'IF))
    (rule 'STATEMENT (list 'FOR))
    (rule 'STATEMENT (list 'WHILE))
    (rule 'STATEMENT (list 'RETURN))

;==============================================================================================
;==== IF rules
;==============================================================================================

    ; IF / ELSE_IF / ELSE
    (rule 'IF (list 'if 'oparen 'COND_ARITH_EXP 'cparen 'STATEMENT_BODY 'ELSE_CLAUSES))
    (rule 'ELSE_IF (list 'else 'if 'oparen 'COND_ARITH_EXP 'cparen 'STATEMENT_BODY 'ELSE_CLAUSES))
    (rule 'ELSE (list 'else 'STATEMENT_BODY))

    ; ELSE_CLASUSE
    (rule 'ELSE_CLAUSES (list 'ELSE_IF))
    (rule 'ELSE_CLAUSES (list 'ELSE))
    (rule 'ELSE_CLAUSES empty)

;==============================================================================================
;==== FOR rules
;==============================================================================================

    ; FOR
    (rule 'FOR (list 'for 'oparen 'FOR_PARAMS 'cparen 'STATEMENT_BODY))

    ; FOR_PARAMS
    (rule 'FOR_PARAMS (list 'FOR_ASSIGN 'semi 'FOR_CLAUSE 'semi 'FOR_ITER))

    ; FOR_ASSIGN
    (rule 'FOR_ASSIGN (list 'VAR_DEFINITION))
    (rule 'FOR_ASSIGN (list 'VAR_ASSIGNMENT))
    (rule 'FOR_ASSIGN empty)

    ; FOR_CLAUSE
    (rule 'FOR_CLAUSE (list 'COND_ARITH_EXP))
    (rule 'FOR_CLAUSE empty)

    ; FOR_ITER
    (rule 'FOR_ITER (list 'VAR_ASSIGNMENT))
    (rule 'FOR_ITER empty)

;==============================================================================================
;==== STATEMENT_BODY rules (if, for, while)
;==============================================================================================

    ; STATEMENT_BODY
    (rule 'STATEMENT_BODY (list 'BLOCK))
    (rule 'STATEMENT_BODY (list 'VAR_ASSIGNMENT 'semi))
    (rule 'STATEMENT_BODY (list 'FUNCTION_CALL_EXP 'semi))
    (rule 'STATEMENT_BODY (list 'THIS_ACCESS 'semi))
    (rule 'STATEMENT_BODY (list 'RETURN))
    
;==============================================================================================
;==== Other Statement Rules: WHILE, RETURN
;==============================================================================================

    (rule 'WHILE (list 'while 'oparen 'COND_ARITH_EXP 'cparen 'STATEMENT_BODY))
    (rule 'RETURN (list 'return 'COND_ARITH_EXP 'semi))
    (rule 'RETURN (list 'return 'void 'semi))
    (rule 'RETURN (list 'return 'semi))

    ; FUNCTION_ARGS
    (rule 'ARGUMENTS (list 'FUNC_ARG_DEF))
    (rule 'ARGUMENTS (list 'FUNC_ARG_CALL))
    (rule 'ARGUMENTS empty)
    (rule 'FUNC_ARG_DEF (list 'FUNC_ARG_DEF 'comma 'TYPE 'id))
    (rule 'FUNC_ARG_DEF (list 'TYPE 'id))
    (rule 'FUNC_ARG_CALL (list 'FUNC_ARG_CALL 'comma 'COND_ARITH_EXP))
    (rule 'FUNC_ARG_CALL (list 'COND_ARITH_EXP))

    ; FUNCTION_CALL (handles: any list of id. or function(). AND ending in function() (eg foo(), a.foo().bar().c.d.f.foo(), a.foo().c.bar(), etc ) )
    (rule 'FUNCTION_CALL_EXP (list 'FUNCTION_CALL_EXP 'dot 'FUNCTION_CALLS))
    (rule 'FUNCTION_CALL_EXP (list 'FUNCTION_CALLS))

    (rule 'FUNCTION_CALLS (list 'IDS 'dot 'FUNCTION_CALL))
    (rule 'FUNCTION_CALLS (list 'FUNCTION_CALL))

    (rule 'FUNCTION_CALL (list 'id 'oparen 'ARGUMENTS 'cparen))

    (rule 'IDS (list 'IDS 'dot 'id))
    (rule 'IDS (list 'id))

    (rule 'THIS_ACCESS (list 'this 'dot 'FUNCTION_CALL_EXP))
    (rule 'THIS_ACCESS (list 'this 'dot 'IDS))
    (rule 'THIS_ACCESS (list 'this))

    (rule 'NEW (list 'NEW_ARRAY))
    (rule 'NEW (list 'NEW_OBJECT))
    (rule 'NEW_ARRAY (list 'new 'PRIMITIVE 'osquare 'COND_ARITH_EXP 'csquare))
    (rule 'NEW_OBJECT (list 'new 'id 'oparen 'ARGUMENTS 'cparen))
    (rule 'NEW_OBJECT (list 'new 'id 'oparen 'ARGUMENTS 'cparen 'dot 'IDS))
    (rule 'NEW_OBJECT (list 'new 'id 'oparen 'ARGUMENTS 'cparen 'dot 'FUNCTION_CALL_EXP))
    (rule 'NEW_OBJECT (list 'new 'id 'oparen 'ARGUMENTS 'cparen 'dot 'FUNCTION_CALL_EXP 'dot 'IDS))

    (rule 'CASTS (list 'CASTS 'CAST))
    (rule 'CASTS (list 'CAST))
    (rule 'CAST (list 'oparen 'PRIMITIVE 'cparen))
    (rule 'CAST (list 'oparen 'PRIMITIVE 'osquare 'csquare 'cparen))
    (rule 'CAST (list 'oparen 'COND_ARITH_EXP 'cparen))
    (rule 'CAST (list 'oparen 'COND_ARITH_EXP 'cparen 'osquare 'csquare))
    (rule 'CAST (list 'oparen 'minus 'COND_ARITH_EXP 'cparen))
    (rule 'CAST (list 'oparen 'minus 'COND_ARITH_EXP 'cparen 'osquare 'csquare))

))

(define operator-rules (append multiplicative-op-rules additive-op-rules relational-op-rules equality-op-rules bitwise-and-op-rules bitwise-or-op-rules logical-and-op-rules logical-or-op-rules assignment-op-rules ))
(define expression-rules (append arithmetic-expression-rules condition-expression-rules))

(define rules (append other-rules scope-rules class-mod-rules type-rules operator-rules expression-rules literal-rules primitive-rules ))

;==============================================================================================
;==== Writing 
;==============================================================================================

(define (string-rhs rhs)
  (cond
    [(empty? rhs) ""]
    [else (string-append " " (symbol->string (first rhs)) (string-rhs (rest rhs)))])) 

(define (rule->string rule)
  (string-append (symbol->string (rule-lhs rule)) (string-rhs (rule-rhs rule))))

(define (print-rule out rule)
  (displayln (rule->string rule) out))

(define (print-list-item out item)
  (cond
    [(symbol? item) (displayln item out)]
    [(rule? item) (print-rule out item)]))

(define (print-list out l)
  (displayln (length l) out)
  (for-each (lambda(item) (print-list-item out item)) l))


(define out (open-output-file "grammar"))

(print-list out terminals)
(print-list out non-terminals)
(displayln start out)
(print-list out rules)  




