#lang racket

(require "class-info.rkt")
(require "ast-tree.rkt")
(require "types.rkt")

(provide gen-code)

(struct stackinfo (mdecls ldecls ebpoff))

(define WORD 4)
(define empty-stackinfo (stackinfo empty empty 0))

(define (get-outfile cinfo)
  (string-append "output/" (foldr string-append "" (info-name cinfo)) ".s"))

(define (gen-code cinfo)
  (define out (open-output-file (get-outfile cinfo) #:exists 'replace))
  (gen-debug-externs out)
  (gen-debug-print-eax out)
  (match (info-ast cinfo)
    [(or (cunit _ _ (class _ _ _ _ _ _ bd))
         (cunit _ _ (interface _ _ _ _ _ bd))) (gen-code-recurse out empty-stackinfo #f bd)]))

(define (gen-code-recurse out sinfo rtnaddr t)
  ;(ast-print-struct t)
  (match t
    [(constructor env sp decl bd) (gen-code-constructor out bd)]
    [(method _ _ '(static) (ptype 'int) (methoddecl _ "test" `()) bd) (gen-code-start-method out bd)]
    [(method env sp md ty (methoddecl _ id params) bd) (gen-code-method out id params bd)]
 ;   [(methoddecl env id params) (methoddecl env id (map F params))]
 ;   [(parameter env type id) (parameter env (F type) id)]
    [(varassign env id ex) (gen-code-varassign out sinfo rtnaddr id ex)]
    [(vdecl env sp md ty id) (gen-code-vdecl out sinfo rtnaddr id)]
    [(binop env op ls rs) (gen-code-binop out sinfo rtnaddr op ls rs)]
    [(unop env op rs) (gen-code-unop out sinfo rtnaddr op rs)]
    [(cast env c ex) (gen-code-cast out sinfo rtnaddr c ex)]
    [(arraycreate env ty sz) (gen-code-arraycreate out sinfo rtnaddr ty sz)]
    [(classcreate env cls params) (gen-code-classcreate out sinfo rtnaddr cls params)]
    [(fieldaccess env left field) (gen-code-fieldaccess out sinfo rtnaddr left field)]
    [(arrayaccess env left index) (gen-code-arrayaccess out sinfo rtnaddr left index)]
    [(while env test body) (gen-code-while out sinfo rtnaddr test body)]
    [(methodcall env left id args) (gen-code-methodcall out sinfo rtnaddr left id args)]
    [(iff env test tru fls) (gen-code-iff out sinfo rtnaddr test tru fls)]
    [(return env expr) (gen-code-return out sinfo rtnaddr expr)]
    [(for env init clause update body) (gen-code-for out sinfo rtnaddr init clause update body)]
;    [(ptype _) ast]
;    [(rtype _) ast]
;    [(atype type) (atype (F type))]
    [(varuse _ id) (gen-code-varuse out sinfo rtnaddr id)]
    [(this _ type) (gen-code-varuse out sinfo rtnaddr type)]
    [(literal _ type val) (gen-code-literal out sinfo rtnaddr type val)]
    [(block env id statements)  (gen-code-block out sinfo rtnaddr id statements) ]
    [`() (comment out "EMPTY STATEMENT")]))

(define (gen-code-block out sinfo rtnaddr id statements)
  (define (gen-code-block-helper sinfo statements)
    (cond
      [(empty? statements) sinfo] ;TODO: pop local vars off stack
      [(varassign? (first statements)) (gen-code-block-helper (gen-code-recurse out sinfo rtnaddr (first statements)) (rest statements))]
      [else (gen-code-recurse out sinfo rtnaddr (first statements))
            (gen-code-block-helper sinfo (rest statements))]))

  (define bsinfo (gen-code-block-helper sinfo statements))
  (reset-stack out (- (length (stackinfo-ldecls bsinfo)) (length (stackinfo-ldecls sinfo)))))

(define (gen-code-constructor out bd)
  (comment out "TODO: generate constructor assembly"))

(define (gen-code-start-method out bd)
  (nl out)
  (comment out "@@@@@@@@@@@@@ ENTRY POINT @@@@@@@@@@@@@")
  (display "global _start\n" out)
  (display "_start:\n" out)
  (gen-code-methodcall out empty-stackinfo #f empty "test" empty)
  (nl out)
  (gen-debug-print out)
  (nl out)
  (display "mov eax, 1\n" out)
  (display "int 0x80\n" out)
  (display "int 3\n" out)
  (comment out "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
  (gen-code-method out "test" empty bd))

(define (gen-code-method out id params bd)
  (define sinfo (stackinfo (get-method-arg-decls (reverse params) 8) empty 4))
  
  (nl out)
  (comment out "######## METHOD " id " ########")
  (label out id "TODO: set the label for this method")
  (push out "ebp")			;save frame pointer
  (mov out "ebp" "esp")			;set new frame pointer to stack pointer
  (gen-code-recurse out sinfo #f bd)	;gen methods code
  (nl out)
  (pop out "ebp")
  (display "ret\n" out)			;ret from method
  (comment out "#############METHOsaD############"))

(define (gen-code-varassign out sinfo rtnaddr id ex)
  (nl out)
  (comment out "VARASSIGN")

  (gen-code-recurse out sinfo #f ex)	;puts result in eax
  (cond
    [(vdecl? id)  (push out "eax" "assign " (vdecl-id id) " to eax")
                  (stackinfo-add-decl sinfo (vdecl-id id))]
    [(varuse? id) (movt out "ebp" "eax" (get-ebp-offset (varuse-id id) sinfo) "assign " (varuse-id id) " to eax")
                   sinfo]
    [(arrayaccess? id) (define temp-sinfo (stackinfo-inc-ebpoff sinfo 1))
                       (push out "ebx" "saving")
                       (mov out "ebx" "eax")
                       (gen-code-recurse out sinfo #t id)
                       (movt out "eax" "ebx" "")
                       (pop out "ebx")
                        sinfo]))

(define (gen-code-vdecl out sinfo rtnaddr id)
  (comment out "TODO: generate vdecl assembly"))
  
(define (gen-code-binop out sinfo rtnaddr op ls rs)
  (define temp-sinfo (stackinfo-inc-ebpoff sinfo 1))
  (push out "ebx" "saving")		;save ebx (cause we gonna use it)
  (comment out "binop " (symbol->string op))

  (cond
    [(or (equal? op 'barbar) (equal? op 'ampamp)) (gen-code-logical out sinfo rtnaddr op ls rs)]
    [else
      (gen-code-recurse out temp-sinfo rtnaddr rs)	;get result of rhs
      (mov out "ebx" "eax" "save binop rs result")			;move result from above into eax
      (gen-code-recurse out temp-sinfo rtnaddr ls)	;get result of lhs
      (match op
        ['plus (add out "eax" "ebx")]
        ['minus (sub out "eax" "ebx")]
        ['star (imul out "eax" "ebx")]
        ['slash (divide out "eax" "ebx")]
	['pct (rem out "eax" "ebx")]
        [(or 'eqeq 'noteq 'gt 'lt 'gteq 'lteq) (gen-code-conditional out sinfo rtnaddr op ls rs)])])

  (pop out "ebx" "restoring"))			;restore ebx

(define (gen-code-conditional out sinfo rtnaddr op ls rs)
  (let ([label-tru (symbol->string (gensym))]	
        [label-end-of-if (symbol->string (gensym))])
    (cmp out "eax" "ebx")				;first compare the 2 operations
    (match op					;then put the correct comditional jmp to a tru label 
      ['eqeq (cjmp out "je" label-tru)]
      ['noteq (cjmp out "jne" label-tru)]
      ['gt (cjmp out "jg" label-tru)]
      ['lt (cjmp out "jl" label-tru)]
      ['gteq (cjmp out "jge" label-tru)]
      ['lteq (cjmp out "jle" label-tru)])
    (movi out "eax" 0)				;if the condition was false set eax to 0
    (jmp out label-end-of-if)
    (label out label-tru)				;and skip passed true code
    (movi out "eax" 1)				;if condition was true set eax to 1
    (label out label-end-of-if)))

(define (gen-code-logical out sinfo rtnaddr op ls rs)
  (let ([label-end (symbol->string (gensym))])
    (movi out "ebx" 1)
    (gen-code-recurse out sinfo rtnaddr ls)
    (cmp out "eax" "ebx")
    (match op
      ['barbar (cjmp out "je" label-end)
               (gen-code-recurse out sinfo rtnaddr rs)
               (label out label-end)]
      ['ampamp (cjmp out "jne" label-end)
               (gen-code-recurse out sinfo rtnaddr rs)
               (label out label-end)])))

(define (gen-code-unop out sinfo rtnaddr op rs)
  (gen-code-recurse out sinfo rtnaddr rs)
  (match op
    ['minus (display "neg eax\n" out)]
    ['not   (display "not eax\n" out)
            (display "and eax,1\n" out)]))

(define (gen-code-cast out sinfo rtnaddr c ex)
  (comment out "TODO: generate cast assembly"))

(define (gen-code-arraycreate out sinfo rtnaddr ty sz)
  (define temp-sinfo (stackinfo-inc-ebpoff sinfo 1))
  (push out "ebx")

  (nl out)
  (comment out "ARRAY CREATE")
  (gen-code-recurse out sinfo rtnaddr sz)
  (mov out "ebx" "eax")		;save the size of the array in ebx
  (addi out "eax" 2)		;add 1 to size so we can store the size
  (call out "__malloc")		;address will be in eax
  (movt out "eax" "ebx" "")	;move the size of the array to the first element
  (nl out)

  (pop out "ebx"))

(define (gen-code-arrayaccess out sinfo rtnaddr left index)
  (define temp-sinfo (stackinfo-inc-ebpoff sinfo 1))
  (push out "ebx")

  (nl out)
  (comment out "ARRAY ACCESS")
  (gen-code-recurse out sinfo rtnaddr index)
  (add out "eax" "2")
  (imul out "eax" "4")
  (mov out "ebx" "eax")			;ebx now holds the index offset
  (gen-code-recurse out sinfo rtnaddr left)	;eax now holds the array address

  (cond
    [rtnaddr (add out "eax" "ebx")]
    [else (movf out "eax" "eax" "+ebx")])

  (nl out)

  (pop out "ebx"))  

(define (gen-code-classcreate out sinfo rtnaddr cls params)
  (comment out "TODO: generate classcreate assembly"))

(define (gen-code-fieldaccess out sinfo rtnaddr left field)
  (comment out "TODO: generate fieldaccess assembly"))

(define (gen-code-methodcall out sinfo rtnaddr left id args)
  (nl out)
  (comment out "=== METHODCALL TO " id " ===")
  (push-method-args out sinfo rtnaddr args)	;push all method args onto stack
  (call out id)				;TODO: call right label
  (reset-stack out (length args))	;"pop" off all the args from the stack
  (comment out "===methodcall end==="))

(define (gen-code-iff out sinfo rtnaddr test tru fls)
  (nl out)
  (comment out "IFF")
  (let  ([label-fls (symbol->string (gensym))]
	[label-end-of-if (symbol->string (gensym))])
    (comment out "Evaluating test")
    (gen-code-recurse out sinfo rtnaddr test)		;eval test, eax will contain 0 or 1 (false or true)
    (comment out "Done evaluating test")
    (nl out)
    (movi out "ecx" 1)				;mov 1 into ecx
    (cmp out "eax" "ecx")			;cmp test to ecx (true)
    (cjmp out "jne" label-fls)			;if test is eqaul to false then jump to the false code
    (nl out)
    (comment out "TRUE CODE")
    (gen-code-recurse out sinfo rtnaddr tru)		;else the test was false so run false code
    (jmp out label-end-of-if)			;done fls code so jump passed tru code
    (comment out "End of true code")		
    (nl out)
    (label out label-fls)
    (comment out "FALSE CODE")
    (gen-code-recurse out sinfo rtnaddr fls)		;tru code
    (comment out "End of false code") 
    (label out label-end-of-if)
    (nl out)))		;end of if statement

(define (gen-code-while out sinfo rtnaddr test body)
  (nl out)
  (comment out "WHILE")
  (let  ([label-cond (symbol->string (gensym))]
         [label-end (symbol->string (gensym))])
    (label out label-cond)
    (gen-code-recurse out sinfo rtnaddr test)
    (movi out "ecx" 1)				
    (cmp out "eax" "ecx")			
    (cjmp out "jne" label-end)			
    (gen-code-recurse out sinfo rtnaddr body)
    (jmp out label-cond)
    (label out label-end)
    (nl out)))

(define (gen-code-for out sinfo rtnaddr init clause update body)
  (let  ([label-cond (symbol->string (gensym))]
         [label-update (symbol->string (gensym))]
         [label-end (symbol->string (gensym))])

    (nl out)
    (comment out "FOR")
    (define new-sinfo (if [varassign? init] (gen-code-recurse out sinfo rtnaddr init) sinfo))
    (jmp out label-cond)
    (label out label-update)
    (gen-code-recurse out new-sinfo rtnaddr update)
    (label out label-cond)
    (gen-code-recurse out new-sinfo rtnaddr clause)
    (movi out "ecx" 1)				
    (cmp out "eax" "ecx")			
    (cjmp out "jne" label-end)			
    (gen-code-recurse out new-sinfo rtnaddr body)
    (jmp out label-update)
    (label out label-end)
    (reset-stack out (- (length (stackinfo-ldecls new-sinfo)) (length (stackinfo-ldecls sinfo))))
    (nl out)))

(define (gen-code-return out sinfo rtnaddr expr)
  (nl out)
  (comment out ";RETURN")
  (gen-code-recurse out sinfo rtnaddr expr)
  (reset-stack out (length (stackinfo-ldecls sinfo)))
  (pop out "ebp")
  (display "ret\n" out)
  (nl out))			;ret from method

(define (gen-code-varuse out sinfo rtnaddr id)
  (comment out "varuse id: " id)
  (movf out "eax" "ebp" (get-ebp-offset id sinfo)))

(define (gen-code-this out sinfo rtnaddr type)
  (comment out "TODO: generate this assembly")) 

(define (gen-code-literal out sinfo rtnaddr type val)
  (match type
    [(ptype 'int) (movi out "eax" val "literal val " (number->string val))]
    [(ptype 'null) (movi out "eax" 0 "literal val null")]
    [(ptype 'boolean) (movi out "eax" (if val 1 0) "literal val null")]))
    ;[(rtype '("java" "lang" "String")) (movi out "eax" val "literal val null")]
    ;[(ptype 'char) (movi out "eax" val "literal val null")]

;==============================================================================================
;==== Helpers
;==============================================================================================

(define (get-ebp-offset id sinfo)
  (define mdecl (assoc id (stackinfo-mdecls sinfo)))
  (define ldecl (assoc id (stackinfo-ldecls sinfo)))
  (cond
    [(and (list? mdecl) (list? ldecl)) (error "We have clashing decls in m and l")]
    [(list? mdecl) (second mdecl)]
    [(list? ldecl) (second ldecl)]
    [else (error "Could find a declaration for a variable? No test should be like that.")]))

(define (stackinfo-add-decl sinfo id)
  (define ldecls (stackinfo-ldecls sinfo))
  (define ebpoff (stackinfo-ebpoff sinfo))
  (stackinfo (stackinfo-mdecls sinfo)
             (cons (list id (string-append "-" (number->string ebpoff))) ldecls)
             (+ ebpoff WORD)))

(define (stackinfo-inc-ebpoff sinfo n)
  (define ldecls (stackinfo-ldecls sinfo))
  (define ebpoff (stackinfo-ebpoff sinfo))
  (stackinfo (stackinfo-mdecls sinfo) ldecls (+ ebpoff (* WORD n))))

(define (stackinfo-dec-ebpoff sinfo n)
  (define ldecls (stackinfo-ldecls sinfo))
  (define ebpoff (stackinfo-ebpoff sinfo))
  (stackinfo (stackinfo-mdecls sinfo) ldecls (- ebpoff (* WORD n))))

(define (reset-stack out n)
  (cond
    [(> n 0) (addi out "esp" (* WORD n))]
    [else (printf "")]))

(define (push-method-args out sinfo rtnaddr args)
  (cond
    [(empty? args) (printf "")]
    [else (gen-code-recurse out sinfo rtnaddr (first args))
          (push out "eax")
          (push-method-args out sinfo rtnaddr (rest args))]))

(define (get-method-arg-decls params ebpoff)
  (cond
    [(empty? params) empty]
    [else (cons (list (parameter-id (first params)) (string-append "+" (number->string ebpoff))) (get-method-arg-decls (rest params) (+ WORD ebpoff)))]))

(define (nl out)
  (display "\n" out))

(define (malloc out nbytes)
  (movi out "eax" nbytes)
  (call out "__malloc"))

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

(define (cmt out lm)
  (display (foldr string-append "" (append (cons "\t;" lm) (list "\n"))) out))

(define (push out reg . comment)
  (display (string-append "push " reg) out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

(define (pop out reg . comment)
  (display (string-append "pop " reg) out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

(define (label out l . comment)
  (display (string-append "" l ":") out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

(define (call out label . comment)
  (display (string-append "call " label) out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

(define (mov out reg1 reg2 . comment)
  (display (string-append "mov " reg1 "," reg2 ) out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

(define (movi out reg i . comment)
  (display (string-append "mov " reg "," (number->string i)) out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

;mov from mem (like lw)
(define (movf out dst src off . comment)
  (display (string-append "mov " dst ",["src off "]") out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

;mov to mem (like sw)
(define (movt out dst src off . comment)
  (display (string-append "mov [" dst off "]," src) out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

(define (sub out reg1 reg2 . comment)
  (display (string-append "sub " reg1 "," reg2) out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

(define (add out reg1 reg2 . comment)
  (display (string-append "add " reg1 "," reg2) out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

(define (addi out reg i . comment)
  (display (string-append "add " reg "," (number->string i)) out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

(define (imul out reg1 reg2 . comment)
  (display (string-append "imul " reg1 "," reg2) out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

(define (cmp out reg1 reg2 . comment)
  (display (string-append "cmp " reg1 "," reg2) out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

(define (jmp out label . comment)
  (display (string-append "jmp " label) out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

(define (cjmp out cj label . comment)
  (display (string-append "" cj " " label) out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))


(define (divide out reg1 reg2)
  (comment out "divide")

 
 
  ;;need to:
  ;;put content of reg2 into eax
  ;;put 0 into edx
  ;;do an idiv
  (if (string=? reg1 "eax") (begin (display (string-append "xchg " reg1 "," reg2 "\n") out)
							   (mov out "ebx" reg2))
			    (begin (mov out "eax" reg2)
			       (mov out "ebx" reg1)))
  (mov out "edx" "0")
  (display (string-append "idiv " "ebx" "\n") out)
  (mov out "eax" "eax"))
  

(define (rem out reg1 reg2)
  (comment out "remainder")

  ;;need to:
  ;;put content of reg2 into eax
  ;;put 0 into edx
  ;;put content of reg1 into ebx
  ;;do an idiv
  (if (string=? reg1 "eax") (begin (display (string-append "xchg " reg1 "," reg2 "\n") out)
							   (mov out "ebx" reg2))
			    (begin (mov out "eax" reg2)
			       (mov out "ebx" reg1)))
  (mov out "edx" "0")
  (display (string-append "idiv " "ebx" "\n") out)
  (mov out "eax" "edx"))


(define (gen-debug-print out)
  (push out "eax")
  (call out "debugwrite")
  (pop out "eax"))

(define (gen-debug-print-eax out)
  (label out "debugwrite")
  (comment out "print eax")
  (display "push ebp\n" out)
  (display "mov ebp,esp\n" out)
  (display "call NATIVEjava.io.OutputStream.nativeWrite\n" out)
  (display "pop ebp\n\n" out)

  (comment out "print newline")
  (display "mov eax,14602\n" out)
  (display "push ebp\n" out)
  (display "mov ebp,esp\n" out)
  (display "call NATIVEjava.io.OutputStream.nativeWrite\n" out)
  (display "pop ebp\n" out)
  (display "ret\n" out))

(define (gen-debug-externs out)
  (display "extern __malloc\n" out)
  (display "extern NATIVEjava.io.OutputStream.nativeWrite\n" out))









