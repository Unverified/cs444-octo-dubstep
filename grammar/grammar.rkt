#lang racket

(require "../token.rkt")

(struct rule (lhs rhs))

(define start 'S)
(define terminals (append (list 'BOF 'EOF 'id) (map first token-exps)))
(define non-terminals (list 'S 'JCLASS 'MOD 'DECLS 'DECL 'FUNC 'VAR 'TYPE 'SCOPE 'ARGS 'ARG_LIST 'ARG 'ASSIGN 'LITERAL 'OBJECT 'CMOD 'VMOD 'FMOD 'CONSTRUCTOR))

(define literal-rules
  (list 
    (rule 'LITERAL (list 'id))
    (rule 'LITERAL (list 'null-lit))
    (rule 'LITERAL (list 'bool-lit))
    (rule 'LITERAL (list 'decimal-lit))
    (rule 'LITERAL (list 'octal-lit))
    (rule 'LITERAL (list 'floating-point-lit))
    (rule 'LITERAL (list 'hex-lit))))

(define type-rules
  (list
    (rule 'TYPE (list 'boolean))
    (rule 'TYPE (list 'int))
    (rule 'TYPE (list 'char))
    (rule 'TYPE (list 'byte))
    (rule 'TYPE (list 'short))
    (rule 'TYPE (list 'OBJECT))
    (rule 'OBJECT (list 'id 'dot 'OBJECT))
    (rule 'OBJECT (list 'id))))

(define mod-rules
  (list
    (rule 'MOD (list 'final))
    (rule 'MOD (list 'static))
    (rule 'MOD (list 'abstract))
    (rule 'MOD empty)))

(define scope-rules
  (list
    (rule 'SCOPE (list 'public))
    (rule 'SCOPE (list 'protected))))

(define other-rules 
   (list 
    (rule 'S (list 'BOF 'JCLASS 'EOF))
    (rule 'JCLASS (list 'public 'MOD 'class 'id 'ocurl 'DECLS 'ccurl))
    (rule 'DECLS (list 'DECLS 'DECL))
    (rule 'DECLS (list 'DECL))
    (rule 'DECLS empty)
    (rule 'DECL (list 'CONSTRUCTOR))
    (rule 'DECL (list 'FUNC))
    (rule 'DECL (list 'VAR))
    (rule 'CONSTRUCTOR (list 'SCOPE 'id 'oparen 'ARGS 'cparen 'ocurl 'ccurl))
    (rule 'FUNC (list 'SCOPE 'MOD 'TYPE 'id 'oparen 'ARGS 'cparen 'ocurl 'ccurl))
    (rule 'ARGS (list 'ARG_LIST))
    (rule 'ARGS empty)
    (rule 'ARG_LIST (list 'ARG 'comma 'ARG_LIST))
    (rule 'ARG_LIST (list 'ARG))
    (rule 'ARG (list 'TYPE 'id))
    (rule 'VAR (list 'SCOPE 'MOD 'TYPE 'ASSIGN 'semi))
    (rule 'ASSIGN (list 'id 'eq 'LITERAL)) 
    (rule 'ASSIGN (list 'id ))))

(define rules (append other-rules scope-rules mod-rules type-rules literal-rules))

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













