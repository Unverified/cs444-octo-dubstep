#lang racket

(require "../token.rkt")

(struct rule (lhs rhs))

(define start 'Sp)
(define terminals (append (list 'BOF 'EOF 'id) (map first token-exps)))
(define non-terminals (list 'Sp 'S 'JCLASS 'MOD 'DECLS 'DECL 'VAR 'TYPE 'ARRAY_TYPE 'OBJECT 'SCOPE 'ARGS 'ARG_LIST 'ARG 'ASSIGN 'LITERAL 'CMOD 'CONSTRUCTOR 'NORMAL_FUNC 'FINAL_FUNC 'STATIC_FUNC 'ABS_FUNC 'FUNC_BODY 'ABS_BODY 'NORMAL_VAR 'STATIC_VAR 'ASSIGN_OP 'NATIVE_FUNC 'NATIVE_BODY 'STATIC_NATIVE 'STATEMENTS 'STATEMENT 'BLOCK 'BLOCK_BODY 'IF 'FOR 'WHILE 'IF_BODY 'CONDITION 'EXPRESSION 'ELSE_CLAUSES 'ELSE_IF 'ELSE 'BOOLEAN 'COND_EXP 'COMP_OP 'BOOL_OP 'ARITH_EXP '2-ARITH_OP '1-ARITH_OP ))

(define literal-rules
  (list 
    (rule 'LITERAL (list 'id))
    (rule 'LITERAL (list 'null-lit))
    (rule 'LITERAL (list 'bool-lit))
    (rule 'LITERAL (list 'decimal-lit))
    (rule 'LITERAL (list 'octal-lit))
    (rule 'LITERAL (list 'floating-point-lit))
    (rule 'LITERAL (list 'hex-lit))))

;==============================================================================================
;==== Operator Rules 
;==============================================================================================

(define comp-op-rules
  (list 
    (rule 'COMP_OP (list 'gt))
    (rule 'COMP_OP (list 'lt))
    (rule 'COMP_OP (list 'gteq))
    (rule 'COMP_OP (list 'lteq))
    (rule 'COMP_OP (list 'eqeq))
    (rule 'COMP_OP (list 'noteq))))

(define boolean-op-rules
  (list 
    (rule 'BOOL_OP (list 'amp))
    (rule 'BOOL_OP (list 'ampamp))
    (rule 'BOOL_OP (list 'bar))
    (rule 'BOOL_OP (list 'barbar))))

(define 2-arithmetic-op-rules
  (list 
    (rule '2-ARITH_OP (list 'plus))
    (rule '2-ARITH_OP (list 'minus))
    (rule '2-ARITH_OP (list 'star))
    (rule '2-ARITH_OP (list 'slash))
    (rule '2-ARITH_OP (list 'carot))
    (rule '2-ARITH_OP (list 'pct))
    (rule '2-ARITH_OP (list 'amp))
    (rule '2-ARITH_OP (list 'bar))
    (rule '2-ARITH_OP (list 'ltlt))
    (rule '2-ARITH_OP (list 'gtgt))
    (rule '2-ARITH_OP (list 'gtgtgt))))

(define 1-arithmetic-op-rules
  (list 
    (rule '1-ARITH_OP (list 'minus))
    (rule '1-ARITH_OP (list 'tilde))))

(define arithmetic-expression-rules
  (list
    (rule 'ARITH_EXP (list 'oparen 'ARITH_EXP 'cparen))
    (rule 'ARITH_EXP (list 'ARITH_EXP '2-ARITH_OP 'ARITH_EXP))
    (rule 'ARITH_EXP (list '1-ARITH_OP 'LITERAL))
    (rule 'ARITH_EXP (list 'LITERAL))))

(define condition-expression-rules
  (list
    (rule 'COND_EXP (list 'oparen 'COND_EXP 'cparen))
    (rule 'COND_EXP (list 'COND_EXP 'BOOL_OP 'COND_EXP))
    (rule 'COND_EXP (list 'not 'COND_EXP))
    (rule 'COND_EXP (list 'ARITH_EXP 'COMP_OP 'ARITH_EXP))
    (rule 'COND_EXP (list 'bool-lit))
    (rule 'COND_EXP (list 'id))))

(define type-rules
  (list
    (rule 'TYPE (list 'boolean))
    (rule 'TYPE (list 'int))
    (rule 'TYPE (list 'char))
    (rule 'TYPE (list 'byte))
    (rule 'TYPE (list 'short))
    (rule 'TYPE (list 'OBJECT))
    (rule 'ARRAY_TYPE (list 'TYPE 'osquare 'csquare))
    (rule 'OBJECT (list 'id 'dot 'OBJECT))
    (rule 'OBJECT (list 'id))))

(define var-func-mod-rules
  (list
    (rule 'MOD (list 'final))
    (rule 'MOD (list 'static))
    (rule 'MOD empty)))

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
    (rule 'Sp (list 'BOF 'S 'EOF))
    (rule 'S (list 'JCLASS))
    (rule 'JCLASS (list 'public 'CMOD 'class 'id 'ocurl 'DECLS 'ccurl))

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
    (rule 'DECL (list 'NORMAL_VAR))
    (rule 'DECL (list 'STATIC_VAR))

;public static native int m(int i);

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
    (rule 'NORMAL_VAR (list 'SCOPE 'TYPE 'ASSIGN))
    (rule 'STATIC_VAR (list 'SCOPE 'static 'TYPE 'ASSIGN))

    (rule 'ASSIGN (list 'ASSIGN_OP))
    (rule 'ASSIGN (list 'id 'semi))
    (rule 'ASSIGN_OP (list 'id 'eq 'LITERAL 'semi))

    ; BLOCK
    (rule 'BLOCK (list 'ocurl 'BLOCK_BODY 'ccurl))
    (rule 'BLOCK_BODY (list 'STATEMENTS))
    (rule 'BLOCK_BODY empty)

    ; STATEMENTS
    (rule 'STATEMENTS (list 'STATEMENTS 'STATEMENT))
    (rule 'STATEMENTS (list 'STATEMENT))

    ; STATEMENT
    (rule 'STATEMENT (list 'NORMAL_VAR))
    (rule 'STATEMENT (list 'STATIC_VAR))
    (rule 'STATEMENT (list 'EXPRESSION))
    (rule 'STATEMENT (list 'IF))
    (rule 'STATEMENT (list 'FOR))
    (rule 'STATEMENT (list 'WHILE))

    ; EXPRESSION
    (rule 'EXPRESSION (list 'ASSIGN_OP))

    ; IF / ELSE_IF / ELSE
    (rule 'IF (list 'if 'oparen 'COND_EXP 'cparen 'IF_BODY 'ELSE_CLAUSES))
    (rule 'ELSE_IF (list 'else 'if 'oparen 'CONDITION 'cparen 'IF_BODY))
    (rule 'ELSE (list 'else 'IF_BODY))

    ; IF_BODY
    (rule 'IF_BODY (list 'EXPRESSION))
    (rule 'IF_BODY (list 'BLOCK))
    (rule 'IF_BODY (list 'semi))

    ; ELSE_CLASUSE
    (rule 'ELSE_CLAUSES (list 'ELSE_IF 'ELSE_CLAUSES))
    (rule 'ELSE_CLAUSES (list 'ELSE))
    (rule 'ELSE_CLAUSES empty)


))

(define operator-rules (append comp-op-rules boolean-op-rules 2-arithmetic-op-rules 1-arithmetic-op-rules))
(define expression-rules (append arithmetic-expression-rules condition-expression-rules))
(define mod-rules (append var-func-mod-rules class-mod-rules))
(define rules (append other-rules scope-rules mod-rules type-rules literal-rules operator-rules arithmetic-expression-rules))

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













