#lang racket

(require "class-info.rkt")
(require "ast-tree.rkt")
(require "types.rkt")

(provide gen-code)

(struct decl (id value soff) #:transparent)

(define stack-off 0)

(define (get-outfile cinfo)
  (string-append "output/" (foldr string-append "" (info-name cinfo)) ".s"))

(define (gen-code cinfo)
  (gen-code-recurse (open-output-file (get-outfile cinfo) #:exists 'replace) empty (info-ast cinfo)))


(define (gen-code-recurse out vdecls t)
  ;(ast-print-struct t)
  (match t
    [(cunit package imports bd) (gen-code-recurse out vdecls bd)]
    [(class env sp md id ex im bd) (gen-code-recurse out vdecls bd)]  
    [(interface env sp md id ex bd) (gen-code-recurse out vdecls bd)]
    [(constructor env sp decl bd) (gen-code-constructor out vdecls bd)]
    [(method _ _ '(static) (ptype 'int) (methoddecl _ "test" `()) bd) (gen-code-start-method out vdecls bd)]
    [(method env sp md ty decl bd) (gen-code-method out vdecls bd)]
 ;   [(methoddecl env id params) (methoddecl env id (map F params))]
 ;   [(parameter env type id) (parameter env (F type) id)]
    [(varassign env id ex) (gen-code-varassign out vdecls id ex)]
    [(vdecl env sp md ty id) (gen-code-vdecl out vdecls id)]
    [(binop env op ls rs) (gen-code-binop out vdecls op ls rs)]
    [(unop env op rs) (gen-code-unop out vdecls op rs)]
    [(cast env c ex) (gen-code-cast out vdecls c ex)]
    [(arraycreate env ty sz) (gen-code-arraycreate out vdecls ty sz)]
    [(classcreate env cls params) (gen-code-classcreate out vdecls cls params)]
    [(fieldaccess env left field) (gen-code-fieldaccess out vdecls left field)]
    [(arrayaccess env left index) (gen-code-arrayaccess out vdecls left index)]
    [(while env test body) (gen-code-while out vdecls test body)]
    [(methodcall env left id args) (gen-code-methodcall out vdecls left id args)]
    [(iff env test tru fls) (gen-code-iff out vdecls test tru fls)]
    [(return env expr) (gen-code-return out vdecls expr)]
    [(for env init clause update body) (gen-code-for out vdecls init clause update body)]
;    [(ptype _) ast]
;    [(rtype _) ast]
;    [(atype type) (atype (F type))]
    [(varuse _ id) (gen-code-varuse out vdecls id)]
    [(this _ type) (gen-code-varuse out vdecls type)]
    [(literal _ type val) (gen-code-literal out vdecls type val)]
    [(block env id statements)  (gen-code-block out vdecls id statements) ]
    [`() (comment out vdecls "EMPTY STATEMENT")]))

(define (gen-code-block out vdecls id statements)
  (comment out "block " (symbol->string id))
  (define (gen-code-block-helper vdecls statements)
    (cond
      [(empty? statements) vdecls] ;TODO: pop local vars off stack
      [(varassign? (first statements)) (gen-code-block-helper (gen-code-recurse out vdecls (first statements)) (rest statements))]
      [else (gen-code-recurse out vdecls (first statements))
            (gen-code-block-helper vdecls (rest statements))]))

  (define bvdecls (gen-code-block-helper vdecls statements))
  (define popn (- (length bvdecls) (length vdecls)))
  (if (> popn 0) (addi out "esp" (* 32 popn)) (printf ""))
  (comment out "end " (symbol->string id)))

(define (gen-code-constructor out vdecls bd)
  (comment out "TODO: generate constructor assembly")
  (gen-code-recurse out vdecls bd))

(define (gen-code-start-method out vdecls bd)
  (display "global _start\n" out)
  (display "_start:\n" out)
  (display "push ebp\n" out)
  (display "mov ebp, esp\n" out)
  (display "call test\n" out)
  (display "pop ebp\n" out)
  (display "mov eax, 1\n" out)
  (display "int 0x80\n" out)
  (display "int 3\n" out)
  (display "test:\n" out)
  (gen-code-method out vdecls bd))

(define (gen-code-method out vdecls bd)
  (comment out "TODO: generate method assembly")
  (gen-code-recurse out vdecls bd))

(define (gen-code-varassign out vdecls id ex)
(printf "vdecls: ~a id: ~a~n" vdecls id)
  (gen-code-recurse out vdecls ex)	;puts result in eax
  (cond
    [(vdecl? id) (push out "eax")	;push the result form above onto the stack
                 (comment out "pushing vdecl " (vdecl-id id))
                 (cons (vdecl-id id) vdecls)]
    [else (mov-stk out "eax" (get-stack-offset (varuse-id id) vdecls 0)) 
          (comment out "assign eax to " (varuse-id id))
           vdecls]))

(define (gen-code-vdecl out vdecls id)
  (comment out "TODO: generate vdecl assembly"))
  
(define (gen-code-binop out vdecls op ls rs)
  (comment out "TODO: generate binop assembly"))

(define (gen-code-unop out vdecls op rs)
  (comment out "TODO: generate unop assembly"))

(define (gen-code-cast out vdecls c ex)
  (comment out "TODO: generate cast assembly"))

(define (gen-code-arraycreate out vdecls ty sz)
  (comment out "TODO: generate arraycreate assembly"))

(define (gen-code-classcreate out vdecls cls params)
  (comment out "TODO: generate classcreate assembly"))

(define (gen-code-fieldaccess out vdecls left field)
  (comment out "TODO: generate fieldaccess assembly"))

(define (gen-code-arrayaccess out vdecls left index)
  (comment out "TODO: generate arrayaccess assembly"))

(define (gen-code-while out vdecls test body)
  (comment out "TODO: generate while assembly"))

(define (gen-code-methodcall out vdecls left id args)
  (comment out "TODO: generate methodcall assembly"))

(define (gen-code-iff out vdecls test tru fls)
  (let  ([label-tru (gensym)]
	[label-fls (gensym)]
	[label-end-of-if (gensym)])
 	(comment out "Evaluating test")
	(gen-code-recurse out vdecls test)
	(comment out "Done evaluating test")

  (comment out "Code to execute on true")
  (display (string-append (symbol->string label-tru) ":\n") out) 
  (gen-code-recurse out vdecls tru)
  (comment out "End of true code")
  (comment out "TODO: Branch on true/false")
  (comment out "On true, skip fls code")
  (display (string-append "jmp " (symbol->string label-end-of-if) "\n") out)
  (comment out "Code to execute on false")
  (display (string-append (symbol->string label-fls) ":\n") out)
 
  (gen-code-recurse out vdecls fls)
  (comment out "End of false code") 
  (display (string-append (symbol->string label-end-of-if) ":\n") out)))

(define (gen-code-return out vdecls expr)
  (comment out "TODO: generate return assembly"))
  
(define (gen-code-for out vdecls init clause update body)
  (comment out "TODO: generate for assembly"))

(define (gen-code-varuse out vdecls id)
  (comment out "TODO: generate varuse assembly"))  

(define (gen-code-this out vdecls type)
  (comment out "TODO: generate this assembly")) 

(define (gen-code-literal out vdecls type val)
  (movi out "eax" val) 
  (comment out "movi " (number->string val) " into eax"))

(define (get-stack-offset id decls soff)
  (cond
    [(empty? decls) (error "Use of variable defore declaration")]
    [(equal? id (first decls)) (+ 32 soff)]
    [else (get-stack-offset id (rest decls) (+ 32 soff))]))

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

(define (push out reg)
  (display (string-append "push " reg) out))

(define (pop out reg)
  (display (string-append "pop " reg) out))

(define (movi out reg i)
  (display (string-append "mov " reg "," (number->string i)) out))

(define (mov-stk out reg soff)
  (display (string-append "mov " reg ",[esp+" (number->string soff) "]") out))

(define (addi out reg i)
  (display (string-append "add " reg "," (number->string i)) out))









