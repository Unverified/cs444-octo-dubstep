#lang racket

(require "class-info.rkt")
(require "ast-tree.rkt")
(require "types.rkt")

(provide gen-code)

(define (get-outfile cinfo)
  (string-append "output/" (foldr string-append "" (info-name cinfo)) ".s"))

(define (gen-code cinfo)
  (gen-code-recurse (open-output-file (get-outfile cinfo)) (info-ast cinfo)))

(define (gen-code-recurse out t)
  (match t
    [(cunit package imports bd) (gen-code-recurse out bd)]
    [(class env sp md id ex im bd) (gen-code-recurse out bd)]  
    [(interface env sp md id ex bd) (gen-code-recurse out bd)]
    [(constructor env sp decl bd) (gen-code-constructor out bd)]
    [(method _ _ '(static) (ptype 'int) (methoddecl _ "test" `()) bd) (gen-code-start-method out bd)]
    [(method env sp md ty decl bd) (gen-code-method out bd)]
 ;   [(methoddecl env id params) (methoddecl env id (map F params))]
 ;   [(parameter env type id) (parameter env (F type) id)]
    [(varassign env id ex) (gen-code-varassign out id ex)]
    [(vdecl env sp md ty id) (gen-code-vdecl out id)]
    [(binop env op ls rs) (gen-code-binop out op ls rs)]
    [(unop env op rs) (gen-code-unop out op rs)]
    [(cast env c ex) (gen-code-cast out c ex)]
    [(arraycreate env ty sz) (gen-code-arraycreate out ty sz)]
    [(classcreate env cls params) (gen-code-classcreate out cls params)]
    [(fieldaccess env left field) (gen-code-fieldaccess out left field)]
    [(arrayaccess env left index) (gen-code-arrayaccess out left index)]
    [(while env test body) (gen-code-while out test body)]
    [(methodcall env left id args) (gen-code-methodcall out left id args)]
    [(iff env test tru fls) (gen-code-iff out test tru fls)]
    [(return env expr) (gen-code-return out expr)]
    [(for env init clause update body) (gen-code-for out init clause update body)]
;    [(ptype _) ast]
;    [(rtype _) ast]
;    [(atype type) (atype (F type))]
    [(varuse _ id) (gen-code-varuse out id)]
    [(this _ type) (gen-code-varuse out type)]
    [(literal _ type val) (gen-code-literal out type val)]
    [(block env id statements) (gen-code-block out id statements)]
    [`() (comment out "EMPTY STATEMENT")]))

(define (gen-code-block out id statements)
  (comment out "block " (symbol->string id))
  (for-each (curry gen-code-recurse out) statements)
  (comment out "end " (symbol->string id)))

(define (gen-code-constructor out bd)
  (comment out "TODO: generate constructor assembly")
  (gen-code-recurse out bd))

(define (gen-code-start-method out bd)
  (display "global _start\n" out)
  (display "_start:\n" out)
  (display "mov eax, 1\n" out)
  (display "int 0x80\n" out)
  (gen-code-method out bd))

(define (gen-code-method out bd)
  (comment out "TODO: generate method assembly")
  (gen-code-recurse out bd))

(define (gen-code-varassign out id ex)
  (comment out "TODO: generate varassign assembly"))

(define (gen-code-vdecl out id)
  (comment out "TODO: generate vdecl assembly"))
  
(define (gen-code-binop out op ls rs)
  (comment out "TODO: generate binop assembly"))

(define (gen-code-unop out op rs)
  (comment out "TODO: generate unop assembly"))

(define (gen-code-cast out c ex)
  (comment out "TODO: generate cast assembly"))

(define (gen-code-arraycreate out ty sz)
  (comment out "TODO: generate arraycreate assembly"))

(define (gen-code-classcreate out cls params)
  (comment out "TODO: generate classcreate assembly"))

(define (gen-code-fieldaccess out left field)
  (comment out "TODO: generate fieldaccess assembly"))

(define (gen-code-arrayaccess out left index)
  (comment out "TODO: generate arrayaccess assembly"))

(define (gen-code-while out test body)
  (comment out "TODO: generate while assembly"))

(define (gen-code-methodcall out left id args)
  (comment out "TODO: generate methodcall assembly"))

(define (gen-code-iff out test tru fls)
  (comment out "TODO: generate if assembly"))

(define (gen-code-return out expr)
  (comment out "TODO: generate return assembly"))
  
(define (gen-code-for out init clause update body)
  (comment out "TODO: generate for assembly"))

(define (gen-code-varuse out id)
  (comment out "TODO: generate varuse assembly"))  

(define (gen-code-this out type)
  (comment out "TODO: generate this assembly")) 

(define (gen-code-literal out type val)
  (comment out "TODO: generate literal assembly"))

;==============================================================================================
;==== Conditions
;==============================================================================================

(define (start-method? t)
  (match t
    [(method _ _ '(static) (ptype 'int) (methoddecl _ "test" `()) _) #t]
    [_ #f]))

;==============================================================================================
;==== x86 Instructions
;==============================================================================================

(define (comment out . m)
  (display (foldr string-append "" (append (cons ";" m) (list "\n"))) out))









