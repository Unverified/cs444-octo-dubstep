#lang racket

(require "../token.rkt")

(struct rule (lhs rhs))

;==============================================================================================
;==== Start 
;==============================================================================================

(define start 'Sp)

;==============================================================================================
;==== Non - Terminals 
;==============================================================================================

(define terminals (append (list 'BOF 'EOF) (remove-duplicates (map first token-exps))))

;==============================================================================================
;==== Terminals 
;==============================================================================================

(define non-terminals (list 'Sp 'S 'PACKAGE 'CLASS_OR_INTERFACE_DECLARATION 'CLASS_DECLARATION 'IDS 'IMPORTS 'IMPORT_LIST 'IMPORT 'CLASS_IMPORT 'PACKAGE_IMPORT 'SCOPE 'EXTENDS 'INTERFACE_BODY 'INTERFACE_DECLARATIONS 'INTERFACE_DECLARATION 'INTERFACE_BODY_DECLARATION 'INTERFACE_BODY_DECLARATIONS 'INTERFACE_BODY_DECLARATIONS_OPT 'IMPLEMENTS 'CLASS_BODY 'IMPLEMENTS_TYPE_LIST 'CLASS_BODY_DECLARATIONS 'CLASS_BODY_DECLARATION 'METHOD_DECLARATOR 'CONSTRUCTOR_DECLARATION 'NORMAL_METHOD_DECLARATION 'FINAL_METHOD_DECLARATION 'STATIC_METHOD_DECLARATION 'ABSTRACT_METHOD_DECLARATION 'STATIC_NATIVE_METHOD_DECLARATION 'STATIC_NATIVE_BODY 'STATIC_NATIVE 'STATIC_CLASS_VARIABLE_DECLARATION 'PARAMETER_LIST 'PARAMETER 'BLOCK 'BLOCK_STATEMENTS 'BLOCK_STATEMENT 'STATEMENT 'STATEMENT_NO_IF 'STATEMENT_WITH_NO_SUB_STATEMENT 'IF_STATEMENT 'IF_ELSE_STATEMENT 'IF_ELSE_STATEMENT_NO_IF 'WHILE_STATEMENT 'WHILE_STATEMENT_NO_IF 'FOR_STATEMENT 'FOR_STATEMENT_NO_IF 'FOR_INIT 'FOR_UPDATE 'EMPTY_STATEMENT 'EXPRESSION_STATEMENT 'STATEMENT_EXPRESSION 'RETURN_STATEMENT 'CLASS_VARIABLE_DECLARATION 'LOCAL_VARAIABLE_DECLARATION 'VARIABLE_DECLARATOR 'EXPRESSION 'LOGICAL_OR 'LOGICAL_AND 'BITWISE_OR 'BITWISE_AND 'EQUALITY 'RELATIONAL 'ADDITIVE 'MULTIPLICATIVE 'UNARY_MINUS 'UNARY_NOT 'CAST 'PRIMARY 'PRIMARY_NO_NEW_ARRAY 'ARRAY_CREATION_EXPRESSION 'FIELD_ACCESS 'METHOD_CALL 'ARGUMENT_LIST 'ARRAY_ACCESS 'ASSIGNMENT 'LHS 'LOGICAL_OR_OP 'LOGICAL_AND_OP 'BITWISE_OR_OP 'BITWISE_AND_OP 'EQUALITY_OP 'RELATIONAL_OP 'ADDITIVE_OP 'MULTIPLICATIVE_OP 'UNARY_MINUS_OP 'UNARY_NOT_OP 'LITERAL 'TYPE 'PRIMITIVE_TYPE 'REFERENCE_TYPE 'ARRAY_TYPE 'METHOD_TYPE 'INTERFACE_MODIFIER 'CLASS_MODIFIER 'EXPRESSION_OPT 'POSTFIX_EXPRESSION 'EXPRESSION_NAME 'BLOCK_STATEMENTS_OPT 'NORMAL_METHOD_DECLARATION_NO_BODY 'VARIABLE_DECLARATOR_OPT 'CLASS_CREATION_EXPRESSION 'ASSIGNMENT_EXPRESSION))

;==============================================================================================
;==== Rules 
;==============================================================================================

(define rules
  (list

;==============================================================================================
;==== Start 
;==============================================================================================

    (rule 'Sp (list 'BOF 'S 'EOF))
    (rule 'S (list 'PACKAGE 'IMPORTS 'CLASS_OR_INTERFACE_DECLARATION))

    (rule 'CLASS_OR_INTERFACE_DECLARATION (list 'CLASS_DECLARATION))
    (rule 'CLASS_OR_INTERFACE_DECLARATION (list 'INTERFACE_DECLARATION))

;==============================================================================================
;==== Package 
;==============================================================================================

    (rule 'PACKAGE (list 'package 'IDS 'semi))
    (rule 'PACKAGE empty)

;==============================================================================================
;==== Import 
;==============================================================================================

    (rule 'IMPORTS (list 'IMPORT_LIST))
    (rule 'IMPORTS empty)

    (rule 'IMPORT_LIST (list 'IMPORT_LIST 'IMPORT))
    (rule 'IMPORT_LIST (list 'IMPORT))

    (rule 'IMPORT (list 'CLASS_IMPORT))
    (rule 'IMPORT (list 'PACKAGE_IMPORT))

    (rule 'CLASS_IMPORT (list 'import 'IDS 'semi))
    (rule 'CLASS_IMPORT (list 'import 'IDS 'dot 'star 'semi))

;==============================================================================================
;==== Interface Declaration
;==============================================================================================

    (rule 'INTERFACE_DECLARATION (list 'public 'INTERFACE_MODIFIER 'interface 'id 'EXTENDS 'INTERFACE_BODY))

;==============================================================================================
;==== Interface Body Declarations
;==============================================================================================

    (rule 'INTERFACE_BODY (list 'ocurl 'INTERFACE_BODY_DECLARATIONS_OPT 'ccurl))

    (rule 'INTERFACE_BODY_DECLARATIONS_OPT (list 'INTERFACE_BODY_DECLARATIONS))
    (rule 'INTERFACE_BODY_DECLARATIONS_OPT empty)

    (rule 'INTERFACE_BODY_DECLARATIONS (list 'INTERFACE_BODY_DECLARATIONS 'INTERFACE_BODY_DECLARATION))
    (rule 'INTERFACE_BODY_DECLARATIONS (list 'INTERFACE_BODY_DECLARATION))

    (rule 'INTERFACE_BODY_DECLARATION (list 'ABSTRACT_METHOD_DECLARATION))
    (rule 'INTERFACE_BODY_DECLARATION (list 'NORMAL_METHOD_DECLARATION_NO_BODY))
    (rule 'NORMAL_METHOD_DECLARATION_NO_BODY (list 'SCOPE 'TYPE 'METHOD_DECLARATOR 'semi))

;==============================================================================================
;==== Class Declaration
;==============================================================================================

    (rule 'CLASS_DECLARATION (list 'public 'CLASS_MODIFIER 'class 'id 'EXTENDS 'IMPLEMENTS 'CLASS_BODY))

    (rule 'EXTENDS (list 'extends 'IDS))
    (rule 'EXTENDS empty)

    (rule 'IMPLEMENTS (list 'implements 'IMPLEMENTS_TYPE_LIST))
    (rule 'IMPLEMENTS empty)

    (rule 'IMPLEMENTS_TYPE_LIST (list 'IMPLEMENTS_TYPE_LIST 'comma 'IDS))
    (rule 'IMPLEMENTS_TYPE_LIST (list 'IDS))

    (rule 'CLASS_BODY (list 'ocurl 'CLASS_BODY_DECLARATIONS 'ccurl))

;==============================================================================================
;==== Class Body Declarations
;==============================================================================================

    (rule 'CLASS_BODY_DECLARATIONS (list 'CLASS_BODY_DECLARATIONS 'CLASS_BODY_DECLARATION))
    (rule 'CLASS_BODY_DECLARATIONS (list 'CLASS_BODY_DECLARATION))

    (rule 'CLASS_BODY_DECLARATION (list 'CONSTRUCTOR_DECLARATION))
    (rule 'CLASS_BODY_DECLARATION (list 'NORMAL_METHOD_DECLARATION))
    (rule 'CLASS_BODY_DECLARATION (list 'FINAL_METHOD_DECLARATION))
    (rule 'CLASS_BODY_DECLARATION (list 'STATIC_METHOD_DECLARATION))
    (rule 'CLASS_BODY_DECLARATION (list 'STATIC_NATIVE_METHOD_DECLARATION))
    (rule 'CLASS_BODY_DECLARATION (list 'ABSTRACT_METHOD_DECLARATION))
    (rule 'CLASS_BODY_DECLARATION (list 'CLASS_VARIABLE_DECLARATION))
    (rule 'CLASS_BODY_DECLARATION (list 'STATIC_CLASS_VARIABLE_DECLARATION))

;==============================================================================================
;==== Constructor Declaration
;==============================================================================================

    (rule 'CONSTRUCTOR_DECLARATION (list 'SCOPE 'METHOD_DECLARATOR 'BLOCK))

;==============================================================================================
;==== Method Declaration
;==============================================================================================

    (rule 'NORMAL_METHOD_DECLARATION (list 'SCOPE 'TYPE 'METHOD_DECLARATOR 'BLOCK))
    (rule 'FINAL_METHOD_DECLARATION (list 'SCOPE 'final 'TYPE 'METHOD_DECLARATOR 'BLOCK))
    (rule 'STATIC_METHOD_DECLARATION (list 'SCOPE 'static 'TYPE 'METHOD_DECLARATOR 'BLOCK))
    (rule 'ABSTRACT_METHOD_DECLARATION (list 'SCOPE 'abstract 'TYPE 'METHOD_DECLARATOR 'semi))
    (rule 'STATIC_NATIVE_METHOD_DECLARATION (list 'SCOPE 'STATIC_NATIVE 'int 'STATIC_NATIVE_BODY 'semi))
    (rule 'STATIC_NATIVE_BODY (list 'id 'oparen 'int 'id 'cparen))
    (rule 'STATIC_NATIVE (list 'static 'native))

    (rule 'METHOD_DECLARATOR (list 'id 'oparen 'PARAMETER_LIST 'cparen))

;==============================================================================================
;==== Parameter List
;==============================================================================================

    (rule 'PARAMETER_LIST (list 'PARAMETER_LIST 'comma 'PARAMETER))
    (rule 'PARAMETER_LIST (list 'PARAMETER))
    (rule 'PARAMETER_LIST empty)
    (rule 'PARAMETER (list 'TYPE 'id))

;==============================================================================================
;==== Block
;==============================================================================================

    (rule 'BLOCK (list 'ocurl 'BLOCK_STATEMENTS_OPT 'ccurl))
 
    (rule 'BLOCK_STATEMENTS_OPT (list 'BLOCK_STATEMENTS))
    (rule 'BLOCK_STATEMENTS_OPT empty)

    (rule 'BLOCK_STATEMENTS (list 'BLOCK_STATEMENTS 'BLOCK_STATEMENT))
    (rule 'BLOCK_STATEMENTS (list 'BLOCK_STATEMENT))

    (rule 'BLOCK_STATEMENT (list 'LOCAL_VARAIABLE_DECLARATION 'semi))
    (rule 'BLOCK_STATEMENT (list 'STATEMENT))
    
;==============================================================================================
;==== Statements
;==============================================================================================

    (rule 'STATEMENT (list 'STATEMENT_WITH_NO_SUB_STATEMENT))
    (rule 'STATEMENT (list 'IF_STATEMENT))
    (rule 'STATEMENT (list 'IF_ELSE_STATEMENT))
    (rule 'STATEMENT (list 'WHILE_STATEMENT))
    (rule 'STATEMENT (list 'FOR_STATEMENT))

    (rule 'STATEMENT_NO_IF (list 'STATEMENT_WITH_NO_SUB_STATEMENT))
    (rule 'STATEMENT_NO_IF (list 'IF_ELSE_STATEMENT_NO_IF))
    (rule 'STATEMENT_NO_IF (list 'WHILE_STATEMENT_NO_IF))
    (rule 'STATEMENT_NO_IF (list 'FOR_STATEMENT_NO_IF))

    (rule 'STATEMENT_WITH_NO_SUB_STATEMENT (list 'BLOCK))
    (rule 'STATEMENT_WITH_NO_SUB_STATEMENT (list 'EMPTY_STATEMENT))
    (rule 'STATEMENT_WITH_NO_SUB_STATEMENT (list 'EXPRESSION_STATEMENT))
    (rule 'STATEMENT_WITH_NO_SUB_STATEMENT (list 'RETURN_STATEMENT))

;==============================================================================================
;==== If Statement
;==============================================================================================

    (rule 'IF_STATEMENT (list 'if 'oparen 'EXPRESSION 'cparen 'STATEMENT))
    (rule 'IF_ELSE_STATEMENT (list 'if 'oparen 'EXPRESSION 'cparen 'STATEMENT_NO_IF 'else 'STATEMENT))
    (rule 'IF_ELSE_STATEMENT_NO_IF (list 'if 'oparen 'EXPRESSION 'cparen 'STATEMENT_NO_IF 'else 'STATEMENT_NO_IF))

;==============================================================================================
;==== While Statement
;==============================================================================================

    (rule 'WHILE_STATEMENT (list 'while 'oparen 'EXPRESSION 'cparen 'STATEMENT))
    (rule 'WHILE_STATEMENT_NO_IF (list 'while 'oparen 'EXPRESSION 'cparen 'STATEMENT_NO_IF))

;==============================================================================================
;==== For Statement
;==============================================================================================

    (rule 'FOR_STATEMENT (list 'for 'oparen 'FOR_INIT 'semi 'EXPRESSION_OPT 'semi 'FOR_UPDATE 'cparen 'STATEMENT))
    (rule 'FOR_STATEMENT_NO_IF (list 'for 'oparen 'FOR_INIT 'semi 'EXPRESSION_OPT 'semi 'FOR_UPDATE 'cparen 'STATEMENT_NO_IF))

    (rule 'FOR_INIT (list 'LOCAL_VARAIABLE_DECLARATION))
    (rule 'FOR_INIT (list 'STATEMENT_EXPRESSION))
    (rule 'FOR_INIT empty)

    (rule 'FOR_UPDATE (list 'STATEMENT_EXPRESSION))
    (rule 'FOR_UPDATE empty)

;==============================================================================================
;==== Other Statements (Empty, Expression Statement, Return)
;==============================================================================================

    (rule 'EMPTY_STATEMENT (list 'semi))

    (rule 'EXPRESSION_STATEMENT (list 'STATEMENT_EXPRESSION 'semi))
    (rule 'STATEMENT_EXPRESSION (list 'ASSIGNMENT))
    (rule 'STATEMENT_EXPRESSION (list 'METHOD_CALL))
    (rule 'STATEMENT_EXPRESSION (list 'CLASS_CREATION_EXPRESSION))

    (rule 'RETURN_STATEMENT (list 'return 'EXPRESSION_OPT 'semi))
    

;==============================================================================================
;==== Variable Declarations (Class and Local)
;==============================================================================================

    (rule 'CLASS_VARIABLE_DECLARATION (list 'SCOPE 'TYPE 'VARIABLE_DECLARATOR_OPT 'semi))
    (rule 'STATIC_CLASS_VARIABLE_DECLARATION (list 'SCOPE 'static 'TYPE 'VARIABLE_DECLARATOR_OPT 'semi))

    (rule 'LOCAL_VARAIABLE_DECLARATION (list 'TYPE 'VARIABLE_DECLARATOR))

    (rule 'VARIABLE_DECLARATOR_OPT (list 'VARIABLE_DECLARATOR))
    (rule 'VARIABLE_DECLARATOR_OPT (list 'id))

    (rule 'VARIABLE_DECLARATOR (list 'id 'eq 'EXPRESSION))


;==============================================================================================
;==== Expression
;==============================================================================================

    (rule 'EXPRESSION (list 'ASSIGNMENT_EXPRESSION))
    (rule 'ASSIGNMENT_EXPRESSION (list 'ASSIGNMENT))
    (rule 'ASSIGNMENT_EXPRESSION (list 'LOGICAL_OR))
    (rule 'LOGICAL_OR (list 'LOGICAL_OR 'LOGICAL_OR_OP 'LOGICAL_AND))
    (rule 'LOGICAL_OR (list 'LOGICAL_AND))
    (rule 'LOGICAL_AND (list 'LOGICAL_AND 'LOGICAL_AND_OP 'BITWISE_OR))
    (rule 'LOGICAL_AND (list 'BITWISE_OR))
    (rule 'BITWISE_OR (list 'BITWISE_OR 'BITWISE_OR_OP 'BITWISE_AND))
    (rule 'BITWISE_OR (list 'BITWISE_AND))
    (rule 'BITWISE_AND (list 'BITWISE_AND 'BITWISE_AND_OP 'EQUALITY))
    (rule 'BITWISE_AND (list 'EQUALITY))
    (rule 'EQUALITY (list 'EQUALITY 'EQUALITY_OP 'RELATIONAL))
    (rule 'EQUALITY (list 'RELATIONAL))
    (rule 'RELATIONAL (list 'RELATIONAL 'instanceof 'REFERENCE_TYPE))
    (rule 'RELATIONAL (list 'RELATIONAL 'RELATIONAL_OP 'ADDITIVE))
    (rule 'RELATIONAL (list 'ADDITIVE))
    (rule 'ADDITIVE (list 'ADDITIVE 'ADDITIVE_OP 'MULTIPLICATIVE))
    (rule 'ADDITIVE (list 'MULTIPLICATIVE))
    (rule 'MULTIPLICATIVE (list 'MULTIPLICATIVE 'MULTIPLICATIVE_OP 'UNARY_MINUS))
    (rule 'MULTIPLICATIVE (list 'UNARY_MINUS))
    (rule 'UNARY_MINUS (list 'UNARY_MINUS_OP 'UNARY_MINUS))
    (rule 'UNARY_MINUS (list 'UNARY_NOT))
    (rule 'UNARY_NOT (list 'UNARY_NOT_OP 'UNARY_MINUS))
    (rule 'UNARY_NOT (list 'POSTFIX_EXPRESSION))
    (rule 'UNARY_NOT (list 'CAST))

    (rule 'POSTFIX_EXPRESSION (list 'PRIMARY))
    (rule 'POSTFIX_EXPRESSION (list 'IDS))

    (rule 'CAST (list 'oparen 'PRIMITIVE_TYPE 'cparen 'UNARY_MINUS))
    (rule 'CAST (list 'oparen 'EXPRESSION 'cparen 'UNARY_NOT))
    (rule 'CAST (list 'oparen 'ARRAY_TYPE 'cparen 'UNARY_NOT))

;==============================================================================================
;==== Expression - Primary
;==============================================================================================

    (rule 'PRIMARY (list 'PRIMARY_NO_NEW_ARRAY))
    (rule 'PRIMARY (list 'ARRAY_CREATION_EXPRESSION))

    (rule 'PRIMARY_NO_NEW_ARRAY (list 'LITERAL))
    (rule 'PRIMARY_NO_NEW_ARRAY (list 'this))
    (rule 'PRIMARY_NO_NEW_ARRAY (list 'oparen 'EXPRESSION 'cparen))
    (rule 'PRIMARY_NO_NEW_ARRAY (list 'FIELD_ACCESS))
    (rule 'PRIMARY_NO_NEW_ARRAY (list 'METHOD_CALL))
    (rule 'PRIMARY_NO_NEW_ARRAY (list 'ARRAY_ACCESS))
    (rule 'PRIMARY_NO_NEW_ARRAY (list 'CLASS_CREATION_EXPRESSION))

    (rule 'ARRAY_CREATION_EXPRESSION (list 'new 'PRIMITIVE_TYPE 'osquare 'EXPRESSION 'csquare))
    (rule 'ARRAY_CREATION_EXPRESSION (list 'new 'IDS 'osquare 'EXPRESSION 'csquare))

    (rule 'CLASS_CREATION_EXPRESSION (list 'new 'IDS 'oparen 'ARGUMENT_LIST 'cparen))

;==============================================================================================
;==== Field Access 
;==============================================================================================

    (rule 'FIELD_ACCESS (list 'PRIMARY 'dot 'id))

;==============================================================================================
;==== Method Call 
;==============================================================================================
  
    (rule 'METHOD_CALL (list 'IDS 'oparen 'ARGUMENT_LIST 'cparen))
    (rule 'METHOD_CALL (list 'FIELD_ACCESS 'oparen 'ARGUMENT_LIST 'cparen))

;==============================================================================================
;==== Argument List
;==============================================================================================

    (rule 'ARGUMENT_LIST (list 'ARGUMENT_LIST 'comma 'EXPRESSION))
    (rule 'ARGUMENT_LIST (list 'EXPRESSION))
    (rule 'ARGUMENT_LIST empty)

;==============================================================================================
;==== Array Access 
;==============================================================================================

    (rule 'ARRAY_ACCESS (list 'IDS 'osquare 'EXPRESSION 'csquare))
    (rule 'ARRAY_ACCESS (list 'PRIMARY_NO_NEW_ARRAY 'osquare 'EXPRESSION 'csquare))

;==============================================================================================
;==== Assignment 
;==============================================================================================

    (rule 'ASSIGNMENT (list 'LHS 'eq 'ASSIGNMENT_EXPRESSION))
    (rule 'LHS (list 'IDS))
    (rule 'LHS (list 'FIELD_ACCESS))
    (rule 'LHS (list 'ARRAY_ACCESS))

;==============================================================================================
;==== Operators 
;==============================================================================================

    (rule 'LOGICAL_OR_OP (list 'barbar))
    (rule 'LOGICAL_AND_OP (list 'ampamp))
    (rule 'BITWISE_OR_OP (list 'bar))
    (rule 'BITWISE_AND_OP (list 'amp))
    (rule 'EQUALITY_OP (list 'eqeq))
    (rule 'EQUALITY_OP (list 'noteq))
    (rule 'RELATIONAL_OP (list 'gt))
    (rule 'RELATIONAL_OP (list 'lt))
    (rule 'RELATIONAL_OP (list 'gteq))
    (rule 'RELATIONAL_OP (list 'lteq))
    (rule 'ADDITIVE_OP (list 'plus))
    (rule 'ADDITIVE_OP (list 'minus))
    (rule 'MULTIPLICATIVE_OP (list 'star))
    (rule 'MULTIPLICATIVE_OP (list 'slash))
    (rule 'MULTIPLICATIVE_OP (list 'pct))
    (rule 'UNARY_MINUS_OP (list 'minus))
    (rule 'UNARY_NOT_OP (list 'not))

;==============================================================================================
;==== Literals 
;==============================================================================================

    (rule 'LITERAL (list 'decimal-lit))
    (rule 'LITERAL (list 'bool-lit))
    (rule 'LITERAL (list 'null-lit))
    (rule 'LITERAL (list 'string-lit))
    (rule 'LITERAL (list 'char-lit))

;==============================================================================================
;==== Types 
;==============================================================================================

    (rule 'EXPRESSION_NAME (list 'EXPRESSION_NAME 'dot 'id))
    (rule 'EXPRESSION_NAME (list 'id))

    (rule 'TYPE (list 'PRIMITIVE_TYPE))
    (rule 'TYPE (list 'REFERENCE_TYPE))
    (rule 'TYPE (list 'void))
    (rule 'PRIMITIVE_TYPE (list 'boolean))
    (rule 'PRIMITIVE_TYPE (list 'byte))
    (rule 'PRIMITIVE_TYPE (list 'short))
    (rule 'PRIMITIVE_TYPE (list 'int))
    (rule 'PRIMITIVE_TYPE (list 'char))
    (rule 'REFERENCE_TYPE (list 'IDS))
    (rule 'REFERENCE_TYPE (list 'ARRAY_TYPE))
    (rule 'ARRAY_TYPE (list 'PRIMITIVE_TYPE 'osquare 'csquare))
    (rule 'ARRAY_TYPE (list 'IDS 'osquare 'csquare))


;==============================================================================================
;==== Scopes
;==============================================================================================

    (rule 'SCOPE (list 'public))
    (rule 'SCOPE (list 'protected))

;==============================================================================================
;==== Modifiers 
;==============================================================================================

    (rule 'CLASS_MODIFIER (list 'final))
    (rule 'CLASS_MODIFIER (list 'static))
    (rule 'CLASS_MODIFIER (list 'abstract))
    (rule 'CLASS_MODIFIER empty)
    (rule 'INTERFACE_MODIFIER (list 'abstract))
    (rule 'INTERFACE_MODIFIER empty)

;==============================================================================================
;==== Other 
;==============================================================================================

    (rule 'IDS (list 'IDS 'dot 'id))
    (rule 'IDS (list 'id))

    (rule 'EXPRESSION_OPT (list 'EXPRESSION))
    (rule 'EXPRESSION_OPT empty)

))

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

