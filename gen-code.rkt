#lang racket

(require "class-info.rkt")
(require "ast-tree.rkt")
(require "types.rkt")

(provide gen-code)

(struct stackinfo (decls edpoff))

(define WORD 4)
(define empty-stackinfo (stackinfo empty 0))

(define (get-outfile cinfo)
  (string-append "output/" (foldr string-append "" (info-name cinfo)) ".s"))

(define (gen-code cinfo)
  (define out (open-output-file (get-outfile cinfo) #:exists 'replace))
  (match (info-ast cinfo)
    [(or (cunit _ _ (class _ _ _ _ _ _ bd))
         (cunit _ _ (interface _ _ _ _ _ bd))) (gen-code-recurse out empty-stackinfo bd)]))

(define (gen-code-recurse out vdecls t)
  ;(ast-print-struct t)
  (match t
    [(constructor env sp decl bd) (gen-code-constructor out bd)]
    [(method _ _ '(static) (ptype 'int) (methoddecl _ "test" `()) bd) (gen-code-start-method out bd)]
    [(method env sp md ty (methoddecl _ id params) bd) (gen-code-method out id params bd)]
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
  (define (gen-code-block-helper vdecls statements)
    (cond
      [(empty? statements) vdecls] ;TODO: pop local vars off stack
      [(varassign? (first statements)) (gen-code-block-helper (gen-code-recurse out vdecls (first statements)) (rest statements))]
      [else (gen-code-recurse out vdecls (first statements))
            (gen-code-block-helper vdecls (rest statements))]))

  (define bvdecls (gen-code-block-helper vdecls statements))
  (reset-stack out (- (length (stackinfo-decls bvdecls)) (length (stackinfo-decls vdecls)))))

(define (gen-code-constructor out bd)
  (comment out "TODO: generate constructor assembly"))

(define (gen-code-start-method out bd)
  (display "global _start\n" out)
  (display "_start:\n" out)
  (gen-code-methodcall out empty-stackinfo empty "test" empty)
  (display "mov eax, 1\n" out)
  (display "int 0x80\n" out)
  (display "int 3\n" out)
  (gen-code-method out "test" empty bd))

(define (gen-code-method out id params bd)
  (define sinfo (stackinfo (get-method-arg-decls params 0) (* WORD (length params))))
  (comment out "METHOD " id)
  (label out id)				;TODO: set the label for this method
  (gen-code-recurse out sinfo bd)	;gen methods code
  (display "ret\n" out))		;ret from method

(define (gen-code-varassign out sinfo id ex)
  (comment out "varassign")
  (gen-code-recurse out sinfo ex)	;puts result in eax

  (cond
    [(vdecl? id) (push out "ebx")		;push the result form above onto the stack
                 (stackinfo-add-decl sinfo (vdecl-id id))]	;-32 since push above inc sp
    [else (mtm out "ebx" (get-ebp-offset (varuse-id id) sinfo 0))
           sinfo]))

(define (gen-code-vdecl out sinfo id)
  (comment out "TODO: generate vdecl assembly"))
  
(define (gen-code-binop out sinfo op ls rs)
  (comment out "binop " (symbol->string op))

  (push out "eax")			;save eax (cause we gonna use it)
  (define temp-sinfo (stackinfo-inc-edpoff sinfo 1))

  (gen-code-recurse out temp-sinfo rs)	;get result of rhs
  (mov out "eax" "ebx")			;move result from above into eax
  (gen-code-recurse out temp-sinfo ls)	;get result of lhs

  (match op
    ['plus (add out "ebx" "eax")]
    ['minus (sub out "ebx" "eax")])

  (pop out "eax"))			;restore eax

(define (gen-code-unop out sinfo op rs)
  (comment out "TODO: generate unop assembly"))

(define (gen-code-cast out sinfo c ex)
  (comment out "TODO: generate cast assembly"))

(define (gen-code-arraycreate out sinfo ty sz)
  (comment out "TODO: generate arraycget-method-arg-decls params edpoffreate assembly"))

(define (gen-code-classcreate out sinfo cls params)
  (comment out "TODO: generate classcreate assembly"))

(define (gen-code-fieldaccess out sinfo left field)
  (comment out "TODO: generate fieldaccess assembly"))

(define (gen-code-arrayaccess out sinfo left index)
  (comment out "TODO: generate arrayaccess assembly"))

(define (gen-code-while out sinfo test body)
  (comment out "TODO: generate while assembly"))

(define (gen-code-methodcall out sinfo left id args)
  (comment out "methodcall to " id)
  (push out "ebp")			;save frame pointer
  (mov out "edp" "esp")			;set new frame pointer to stack pointer
  (push-method-args out sinfo args)	;push all method args onto stack
  (call out id)				;TODO: call right label
  (reset-stack out (length args))	;"pop" off all the args from the stack
  (pop out "edp"))			;restore frame pointer

(define (gen-code-iff out sinfo test tru fls)
  (let  ([label-tru (gensym)]
	[label-fls (gensym)]
	[label-end-of-if (gensym)])
  (comment out "Evaluating test")
  (gen-code-recurse out sinfo test)
  (comment out "Done evaluating test")
  (comment out "TODO: Branch on true/false")
  (comment out "Code to execute on true")
  (display (string-append (symbol->string label-tru) ":\n") out) 
  (gen-code-recurse out sinfo tru)
  (comment out "End of true code")

  (comment out "On true, skip fls code")
  (display (string-append "jmp " (symbol->string label-end-of-if) "\n") out)
  (comment out "Code to execute on false")
  (display (string-append (symbol->string label-fls) ":\n") out)
 
  (gen-code-recurse out sinfo fls)
  (comment out "End of false code") 
  (display (string-append (symbol->string label-end-of-if) ":\n") out)))

(define (gen-code-return out sinfo expr)
  (comment out "TODO: generate return assembly"))
  
(define (gen-code-for out sinfo init clause update body)
  (comment out "TODO: generate for assembly"))

(define (gen-code-varuse out sinfo id)
  (comment out "varuse id: " id)
  (mfm out "ebx" (get-ebp-offset id sinfo 0)))

(define (gen-code-this out sinfo type)
  (comment out "TODO: generate this assembly")) 

(define (gen-code-literal out sinfo type val)
  (comment out "literal val " (number->string val))
  (movi out "ebx" val))

(define (get-ebp-offset id sinfo soff)
  (second (assoc id (stackinfo-decls sinfo))))

;==============================================================================================
;==== Helpers
;==============================================================================================

(define (stackinfo-add-decl sinfo id)
  (define decls (stackinfo-decls sinfo))
  (define edpoff (stackinfo-edpoff sinfo))
  (stackinfo (cons (list id edpoff) decls)
             (+ edpoff WORD)))

(define (stackinfo-inc-edpoff sinfo n)
  (define decls (stackinfo-decls sinfo))
  (define edpoff (stackinfo-edpoff sinfo))
  (stackinfo decls (+ edpoff (* WORD n))))

(define (stackinfo-dec-edpoff sinfo n)
  (define decls (stackinfo-decls sinfo))
  (define edpoff (stackinfo-edpoff sinfo))
  (stackinfo decls (- edpoff (* WORD n))))

(define (reset-stack out n)
  (cond
    [(> n 0) (addi out "esp" (* WORD n))]
    [else (printf "")]))

(define (push-method-args out sinfo args)
  (cond
    [(empty? args) (printf "")]
    [else (gen-code-recurse out sinfo (first args))
          (push out "ebx")
          (push-method-args out sinfo (rest args))]))

(define (get-method-arg-decls params edpoff)
  (cond
    [(empty? params) empty]
    [else (cons (list (parameter-id (first params)) edpoff) (get-method-arg-decls (rest params) (+ WORD edpoff)))]))

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
  (display (string-append "\tpush " reg "\n") out))

(define (pop out reg)
  (display (string-append "\tpop " reg "\n") out))

(define (label out l)
  (display (string-append "\t" l ":\n") out))

(define (call out label)
  (display (string-append "\tcall " label "\n") out))

(define (mov out reg1 reg2)
  (display (string-append "\tmov " reg1 "," reg2 "\n") out))

(define (movi out reg i)
  (display (string-append "\tmov " reg "," (number->string i) "\n") out))

;mov from mem (like lw)
(define (mfm out reg soff)
  (display (string-append "\tmov " reg ",[ebp" (if [> soff 0] (string-append "-" (number->string soff)) "") "]" "\n") out))

;mov to mem (like sw)
(define (mtm out reg soff)
  (display (string-append "\tmov [ebp" (if [> soff 0] (string-append "-" (number->string soff)) "") "]," reg "\n") out))

(define (sub out reg1 reg2)
  (display (string-append "\tsub " reg1 "," reg2 "\n") out))

(define (add out reg1 reg2)
  (display (string-append "\tadd " reg1 "," reg2 "\n") out))

(define (addi out reg i)
  (display (string-append "\tadd " reg "," (number->string i) "\n") out))

;will output asm code to print a char based on the value in reg. Just fyi, 14357 will dispaly 'A' for some reason.
(define (print-reg out reg)
  (display (string-append "mov eax," reg) out)
  (display "push ebp" out)
  (display "mov ebp,esp" out)
  (display "push [ebp]" out)
  (display "call NATIVEjava.io.OutputStream.nativeWrite" out)
  (display "add esp,4" out)
  (display "pop ebp" out))








