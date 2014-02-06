#lang racket

(require "../token.rkt")

(struct rule (lhs rhs))

(define start 'Sp)
(define terminals (append (list 'BOF 'EOF 'id) (map first token-exps)))
(define non-terminals (list 'Sp 'S 'JCLASS 'DECLS 'DECL 'TYPE 'ARRAY_TYPE 'OBJECT 'SCOPE 'ARGS 'ARG_LIST 'ARG 'LITERAL 'CMOD 'CONSTRUCTOR 'NORMAL_FUNC 'FINAL_FUNC 'STATIC_FUNC 'ABS_FUNC 'FUNC_BODY 'ABS_BODY 'MEMBER_VAR 'STATIC_MEMBER_VAR 'MEMBER_VAR_DEF 'NATIVE_FUNC 'NATIVE_BODY 'STATIC_NATIVE 'STATEMENTS 'STATEMENT 'BLOCK 'BLOCK_BODY 'IF 'FOR 'FOR_PARAMS 'FOR_ASSIGN 'FOR_CLAUSE 'FOR_ITER 'WHILE 'STATEMENT_BODY 'ELSE_CLAUSES 'ELSE_IF 'ELSE 'COND_ARITH_EXP 'ARITH_EXP 'ARITH_TERM 'ARITH_FACTOR 'ASSIGNMENT_OP 'LOGICAL_OR_OP 'LOGICAL_AND_OP 'BITWISE_OR_OP 'BITWISE_AND_OP 'EQUALITY_OP 'RELATIONAL_OP 'MULT_OP 'ADDITIVE_OP 'CP5 'CP4 'CP3 'CP2 'CP1 'CP0 'EXPRESSION 'VAR_DEFINITION 'VAR_ASSIGNMENT 'FUNC_CALL 'FUNC_ARGS 'FUNC_ARG_LIST 'RETURN 'CLASS_ADDITION 'PACKAGE 'PACKAGE_IMPORT 'IMPORTS 'IMPORT 'CALL_CHANG 'FUNC_CALLS 'FUNC_CALL_STATIC 'CLASS_IMPORT 'OBJECTS 'FIELD_ACCESS_EXP 'FIELD_ACCESS))

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
    (rule 'RELATIONAL_OP (list 'gteq))
    (rule 'RELATIONAL_OP (list 'instanceof))))

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
    (rule 'COND_ARITH_EXP (list 'COND_ARITH_EXP 'LOGICAL_OR_OP 'CP5))
    (rule 'COND_ARITH_EXP (list 'CP5))
    (rule 'CP5 (list 'CP5 'LOGICAL_AND_OP 'CP4))
    (rule 'CP5 (list 'CP4))
    (rule 'CP4 (list 'CP4 'BITWISE_OR_OP 'CP3))
    (rule 'CP4 (list 'CP3))
    (rule 'CP3 (list 'CP3 'BITWISE_AND_OP 'CP2))
    (rule 'CP3 (list 'CP2))
    (rule 'CP2 (list 'CP2 'EQUALITY_OP 'CP1))
    (rule 'CP2 (list 'CP1))
    (rule 'CP1 (list 'CP1 'not 'CP0))
    (rule 'CP1 (list 'CP0))
    (rule 'CP0 (list 'CP0 'RELATIONAL_OP 'ARITH_EXP))
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
    (rule 'ARITH_FACTOR (list 'oparen 'COND_ARITH_EXP 'cparen))		
    (rule 'ARITH_FACTOR (list 'LITERAL))		
    (rule 'ARITH_FACTOR (list 'id))
    (rule 'ARITH_FACTOR (list 'FIELD_ACCESS_EXP))		
    (rule 'ARITH_FACTOR (list 'FUNC_CALLS))
))

(define type-rules
  (list
    (rule 'TYPE (list 'boolean))
    (rule 'TYPE (list 'int))
    (rule 'TYPE (list 'char))
    (rule 'TYPE (list 'byte))
    (rule 'TYPE (list 'short))
    (rule 'TYPE (list 'OBJECT))
    (rule 'ARRAY_TYPE (list 'TYPE 'osquare 'csquare))
    (rule 'OBJECT (list 'OBJECT 'dot 'id))
    (rule 'OBJECT (list 'id))))

(define class-mod-rules
  (list
    (rule 'CMOD (list 'final))
    (rule 'CMOD (list 'static))
    (rule 'CMOD (list 'abstract))
    (rule 'CMOD empty)))

(define scope-rules
  (list
    (rule 'SCOPE (list 'public))
    (rule 'SCOPE (list 'protected))))

(define other-rules 
   (list 
    ; Start
    (rule 'Sp (list 'BOF 'S 'EOF))
    (rule 'S (list 'PACKAGE 'IMPORTS 'JCLASS))

    ; JCLASS
    (rule 'PACKAGE (list 'package 'OBJECT 'semi))
    (rule 'PACKAGE empty)

    ; JCLASS
    (rule 'IMPORTS (list 'IMPORTS 'IMPORT))
    (rule 'IMPORTS empty)
    (rule 'IMPORT (list 'import 'CLASS_IMPORT 'semi))
    (rule 'IMPORT (list 'import 'PACKAGE_IMPORT 'semi))
    (rule 'CLASS_IMPORT (list 'CLASS_IMPORT 'dot 'id))
    (rule 'PACKAGE_IMPORT (list 'CLASS_IMPORT 'dot 'star))

    ; JCLASS
    (rule 'JCLASS (list 'public 'CMOD 'class 'id 'CLASS_ADDITION 'ocurl 'DECLS 'ccurl))

    ; CLASS_ADDITION
    (rule 'CLASS_ADDITION (list 'extends 'id))
    (rule 'CLASS_ADDITION (list 'implements 'id))
    (rule 'CLASS_ADDITION empty)

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
    (rule 'FUNC_BODY (list 'id 'oparen 'ARGS 'cparen 'BLOCK))
    (rule 'ABS_BODY (list 'id 'oparen 'ARGS 'cparen 'semi))
    (rule 'NATIVE_BODY (list 'id 'oparen 'int 'id 'cparen 'semi))

    ; ARGS
    (rule 'ARGS (list 'ARG_LIST))
    (rule 'ARGS empty)
    (rule 'ARG_LIST (list 'ARG 'comma 'ARG_LIST))
    (rule 'ARG_LIST (list 'ARG))
    (rule 'ARG (list 'TYPE 'id))

    ; Variable rules
    (rule 'MEMBER_VAR (list 'SCOPE 'MEMBER_VAR_DEF 'semi))
    (rule 'STATIC_MEMBER_VAR (list 'SCOPE 'static 'MEMBER_VAR_DEF 'semi))

    ; Variable assigment
    (rule 'MEMBER_VAR_DEF (list 'TYPE 'id))
    (rule 'MEMBER_VAR_DEF (list 'VAR_DEFINITION))
    (rule 'VAR_DEFINITION (list 'TYPE 'VAR_ASSIGNMENT))
    (rule 'VAR_ASSIGNMENT (list 'id 'eq 'COND_ARITH_EXP))

    ; FUNC_CALL (handles: foo() / foo().bar().etc / foo().a / foo().a.bar() / etc )
    (rule 'FUNC_CALLS (list 'FUNC_CALLS 'dot 'FUNC_CALL))
    (rule 'FUNC_CALLS (list 'FUNC_CALLS 'dot 'id))
    (rule 'FUNC_CALLS (list 'FUNC_CALL))
    (rule 'FUNC_CALL (list 'id 'oparen 'FUNC_ARGS 'cparen))

    ; FUNC_ARGS
    (rule 'FUNC_ARGS (list 'FUNC_ARG_LIST))
    (rule 'FUNC_ARGS empty)
    (rule 'FUNC_ARG_LIST (list 'LITERAL 'comma 'FUNC_ARG_LIST))
    (rule 'FUNC_ARG_LIST (list 'LITERAL))
   
    ; FIELD_ACCESS_EXP (handles: a.b / a.b.c / etc / a.FUNC_CALLS / a.b.FUNC_CALLS / etc )
    (rule 'FIELD_ACCESS_EXP (list 'FIELD_ACCESS 'FUNC_CALL))
    (rule 'FIELD_ACCESS_EXP (list 'FIELD_ACCESS 'id))
    (rule 'FIELD_ACCESS (list 'FIELD_ACCESS 'id 'dot))
    (rule 'FIELD_ACCESS (list 'id 'dot))


   ; (rule 'FIELD_ACCESS_EXP (list 'FIELD_ACCESS_EXP 'id 'dot 'FIELD_ACCESS))
   ; (rule 'FIELD_ACCESS_EXP (list 'id 'dot))
   ; (rule 'FIELD_ACCESS (list 'FUNC_CALLS))
   ; (rule 'FIELD_ACCESS (list 'id))

    ; EXPRESSION
    (rule 'EXPRESSION (list 'VAR_DEFINITION 'semi))
    (rule 'EXPRESSION (list 'VAR_ASSIGNMENT 'semi))
    (rule 'EXPRESSION (list 'FUNC_CALLS 'semi))

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
    (rule 'STATEMENT (list 'FUNC_CALLS 'semi))
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
    (rule 'ELSE_IF (list 'else 'if 'oparen 'COND_ARITH_EXP 'cparen 'STATEMENT_BODY))
    (rule 'ELSE (list 'else 'STATEMENT_BODY))

    ; ELSE_CLASUSE
    (rule 'ELSE_CLAUSES (list 'ELSE_IF 'ELSE_CLAUSES))
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
    (rule 'STATEMENT_BODY (list 'FUNC_CALLS 'semi))
    
;==============================================================================================
;==== Other Statement Rules: WHILE, RETURN
;==============================================================================================

    (rule 'WHILE (list 'while 'oparen 'COND_ARITH_EXP 'cparen 'STATEMENT_BODY))
    (rule 'RETURN (list 'return 'COND_ARITH_EXP))

))

(define operator-rules (append multiplicative-op-rules additive-op-rules relational-op-rules equality-op-rules bitwise-and-op-rules bitwise-or-op-rules logical-and-op-rules logical-or-op-rules assignment-op-rules ))
(define expression-rules (append arithmetic-expression-rules condition-expression-rules))

(define rules (append other-rules scope-rules class-mod-rules type-rules operator-rules expression-rules literal-rules))

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













