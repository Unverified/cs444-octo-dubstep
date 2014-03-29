#lang racket

(require "class-info.rkt")
(require "ast-tree.rkt")
(require "types.rkt")

(require "gen-headers.rkt")
(require "environments.rkt")
(require "generation-structures.rkt")
(require "mangle-names.rkt")

(provide gen-code)

(struct stackinfo (mdecls ldecls sdecls ebpoff))

(define WORD 4)
(define empty-stackinfo (stackinfo empty empty empty 0))

(define (get-outfile cinfo)
  (string-append "output/" (foldr string-append "" (info-name cinfo)) ".s"))

(define (gen-code cinfo cenvs)
  (define out (open-output-file (get-outfile cinfo) #:exists 'replace))
  (define cenv (find-codeenv (info-name cinfo) cenvs))
  (define entry-label (mangle-names (find-codemeth (funt "test" empty) (codeenv-methods cenv))))
  (gen-static out cenv)
  (display "\n\n\nsection .text\n" out)
  (gen-debug-externs out)
  (gen-debug-print-eax out)
  (for-each(lambda (x)
             (for-each (curryr display out) (list  (mangle-names x) ":\t; Method Def - " (funt-id (codemeth-id x)) "\n"))
             (match (codemeth-def x)
               [(constructor env sp (methoddecl _ id params) bd) (gen-code-constructor out (codeenv-parent cenv) params bd cenvs)]
               [(method _ _ '(static) (ptype 'int) (methoddecl _ "test" `()) bd) (gen-code-start-method out entry-label bd cenvs)]
               [(method env sp md ty (methoddecl _ id params) bd) (gen-code-method out params bd cenvs)])) 
    (filter codemeth-ref? (codeenv-methods cenv))))


(define (gen-code-recurse out sinfo t cenvs)
  ;(ast-print-struct t)
  (match t
    [(varassign env id ex) (gen-code-varassign out sinfo id ex cenvs)]
    [(binop env op ls rs) (gen-code-binop out sinfo op ls rs cenvs)]
    [(unop env op rs) (gen-code-unop out sinfo op rs cenvs)]
    [(cast env c ex) (gen-code-cast out sinfo c ex cenvs)]
    [(arraycreate env ty sz) (gen-code-arraycreate out sinfo ty sz cenvs)]
    [(classcreate env cls params) (gen-code-classcreate out sinfo cls params cenvs)]
    [(fieldaccess env left field) (gen-code-fieldaccess out sinfo left field cenvs)]
    [(arrayaccess env left index) (gen-code-arrayaccess out sinfo #f left index cenvs)]
    [(while env test body) (gen-code-while out sinfo test body cenvs)]
    [(methodcall env left id args) (gen-code-methodcall out sinfo left id args cenvs)]
    [(iff env test tru fls) (gen-code-iff out sinfo test tru fls cenvs)]
    [(return env expr) (gen-code-return out sinfo expr cenvs)]
    [(for env init clause update body) (gen-code-for out sinfo init clause update body cenvs)]
;    [(ptype _) ast]
;    [(rtype _) ast]
;    [(atype type) (atype (F type))]
    [(varuse _ id) (gen-code-varuse out sinfo id cenvs)]
    [(this _ type) (thunk (gen-code-varuse out sinfo "this" cenvs))]
    [(literal _ type val) (gen-code-literal out sinfo type val cenvs)]
    [(block env id statements)  (gen-code-block out sinfo id statements cenvs) ]
    [`() (comment out "EMPTY STATEMENT")]))

;==============================================================================================
;==== Block Generation
;==============================================================================================

(define (gen-code-block out sinfo id statements cenvs)
  (define (gen-code-block-helper sinfo statements)
    (cond
      [(empty? statements) sinfo] ;TODO: pop local vars off stack
      [(varassign? (first statements)) (gen-code-block-helper (gen-code-recurse out sinfo (first statements) cenvs) (rest statements))]
      [else (gen-code-recurse out sinfo (first statements) cenvs)
            (gen-code-block-helper sinfo (rest statements))]))

  (define bsinfo (gen-code-block-helper sinfo statements))
  (reset-stack out (- (length (stackinfo-ldecls bsinfo)) (length (stackinfo-ldecls sinfo)))))

;==============================================================================================
;==== Variable Assignment Generation
;==============================================================================================

(define (gen-code-varassign out sinfo id ex cenvs)
  (nl out)
  (comment out "VARASSIGN")

  (gen-code-recurse out sinfo ex cenvs)	;puts result in eax
  (cond
    [(vdecl? id)  (push out "eax" "assign " (vdecl-id id) " to eax")
                  (stackinfo-add-sdecl (stackinfo-add-ldecl sinfo (vdecl-id id)) ex)]
    [(varuse? id) (movt out "ebp" "eax" (get-ebp-offset (varuse-id id) sinfo) "assign " (varuse-id id) " to eax")
                   sinfo]
    [(arrayaccess? id) (push out "ebx" "saving")
                       (mov out "ebx" "eax")
                       (gen-code-arrayaccess out sinfo #t (arrayaccess-left id) (arrayaccess-index id) cenvs)
                       (movt out "eax" "ebx" "")
                       (pop out "ebx")
                        sinfo]))

;==============================================================================================
;==== Class Generation
;==============================================================================================

(define (gen-code-constructor out parent params bd cenvs)
  (comment out "CONSTRUCTOR")
  (if [empty? parent] (printf "") (call out (constr-label parent params)))
  (gen-code-method out empty bd cenvs)
  (nl out))

(define (gen-code-classcreate out sinfo cls params cenvs)
  (comment out "TODO: generate classcreate assembly, " (foldr string-append "" cls)))

;==============================================================================================
;==== Method Generation
;==============================================================================================

;ENTRY POINT
(define (gen-code-start-method out entry-label bd cenvs)
  (nl out)
  (gen-code-method out empty bd cenvs)
  (comment out "@@@@@@@@@@@@@ ENTRY POINT @@@@@@@@@@@@@")
  (display "global _start\n" out)
  (display "_start:\n" out)
  (call out entry-label)
  (nl out)
  (gen-debug-print out)
  (nl out)
  (display "mov eax, 1\n" out)
  (display "int 0x80\n" out)
  (display "int 3\n" out)
  (comment out "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"))

;METHOD DECLARATION
(define (gen-code-method out params bd cenvs)
  (define sinfo (stackinfo (get-method-arg-decls (reverse params) 12) empty empty 4))

  (comment out "####### METHOD ######")  
  (push out "ebp")			;save frame pointer
  (mov out "ebp" "esp")			;set new frame pointer to stack pointer
  (gen-code-recurse out sinfo bd cenvs)	;gen methods code
  (nl out)
  (pop out "ebp")
  (display "ret\n" out)			;ret from method
  (comment out "#############METHOsaD############"))

;METHOD CALL
(define (gen-code-methodcall out sinfo left id args cenvs)
  (nl out)
  (push out "ebx")
  (comment out "=== METHODCALL TO " id " ===")
  (gen-code-get-this out sinfo left cenvs)
  (comment out "Pushing method args on stack")
  (push-method-args out sinfo args cenvs)	;push all method args onto stack
  (push out "ebx" "this") 		;push the addr of "this"
  (call out id)				;TODO: call right label
  (reset-stack out (length args))	;"pop" off all the args from the stack
  (comment out "===methodcall end===")
  (pop out "ebx"))

;METHOD RETURN
(define (gen-code-return out sinfo expr cenvs)

  (nl out)
  (comment out ";RETURN")
  (gen-code-recurse out sinfo expr cenvs)
  (reset-stack out (length (stackinfo-ldecls sinfo)))
  (pop out "ebp")
  (display "ret\n" out)
  (nl out))			;ret from method

;------- Method Helpers -------

(define (reset-stack out n)
  (cond
    [(> n 0) (addi out "esp" (* WORD n))]
    [else (printf "")]))

(define (push-method-args out sinfo args cenvs)
  (cond
    [(empty? args) (printf "")]
    [else (gen-code-recurse out sinfo (first args) cenvs)
          (push out "eax")
          (push-method-args out sinfo (rest args) cenvs)]))

(define (get-method-arg-decls params ebpoff)
  (cond
    [(empty? params) empty]
    [else (cons (list (parameter-id (first params)) (string-append "+" (number->string ebpoff))) 
                (get-method-arg-decls (rest params) (+ WORD ebpoff)))]))

;==============================================================================================
;==== Operation Generation
;==============================================================================================

;UNOP
(define (gen-code-unop out sinfo op rs cenvs)
  (gen-code-recurse out sinfo rs cenvs)
  (match op
    ['minus (display "neg eax\n" out)]
    ['not   (display "not eax\n" out)
            (display "and eax,1\n" out)]))

;BINOP
(define (gen-code-binop out sinfo op ls rs cenvs)
  (push out "ebx" "saving")		;save ebx (cause we gonna use it)
  (comment out "binop " (symbol->string op))

  (cond
    [(or (equal? op 'barbar) (equal? op 'ampamp)) (gen-code-logical out sinfo op ls rs cenvs)]
    [(equal? op 'instanceof) (gen-code-instanceof out sinfo ls rs cenvs)]
    [else
      (if [stringlit? rs] (gen-code-stringlit out sinfo rs cenvs) (gen-code-recurse out sinfo rs cenvs))
      (mov out "ebx" "eax" "save binop rs result")			;move result from above into eax
      (if [stringlit? ls] (gen-code-stringlit out sinfo ls cenvs) (gen-code-recurse out sinfo ls cenvs))
      (match op
        ['plus (add out "eax" "ebx")]
        ['minus (sub out "eax" "ebx")]
        ['star (imul out "eax" "ebx")]
        ['slash (divide out "eax" "ebx")]
	['pct (rem out "eax" "ebx")]
        [(or 'eqeq 'noteq 'gt 'lt 'gteq 'lteq) (gen-code-conditional out sinfo op ls rs cenvs)])])

  (pop out "ebx" "restoring"))			;restore ebx

;Helper - CONDITIONAL
(define (gen-code-conditional out sinfo op ls rs cenvs)
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

;Helper - LOGICAL
(define (gen-code-logical out sinfo op ls rs cenvs)
  (let ([label-end (symbol->string (gensym))])
    (movi out "ebx" 1)
    (gen-code-recurse out sinfo ls cenvs)
    (cmp out "eax" "ebx")
    (match op
      ['barbar (cjmp out "je" label-end)
               (gen-code-recurse out sinfo rs cenvs)
               (label out label-end)]
      ['ampamp (cjmp out "jne" label-end)
               (gen-code-recurse out sinfo rs cenvs)
               (label out label-end)])))

;;helper - INSTANCEOF
(define (gen-code-instanceof out sinfo ls rs cenvs)
	(gen-code-recurse out sinfo ls cenvs)
	(let*
		([fail-label (gensym "instanceof-fail")]
		 [success-label (gensym "instanceof-success")]
		 [end-label (gensym "instanceof-end")]
  		 [name (rtype-type rs)]
		 [cenv (find-codeenv name cenvs)] )
		
		(cond
			[(codeenv-class? cenv)
		  		(gen-get-class-id out "eax")
		  		;;get id list
		  		(let ([id-list (codeenv-casts cenv)])
			  		(gen-check-if-castable out id-list "eax" "ebx" fail-label success-label))
				(label out fail-label)
				(movi out "eax" "0")
				(jmp out end-label)
				(label out success-label)
				(movi out "eax" "1")
				(label out end-label)]
			[else (error 'cast-rtype-interface "unimplemented")])))

(define (stringlit? t)
  (and (literal? t) (equal? (literal-type t) (rtype '("java" "lang" "String")))))



(define (gen-code-stringlit out sinfo slit cenvs)
  (match (assoc (literal-value slit) (stackinfo-sdecls sinfo))
    [(list key value) (movf out "eax" "ebp" value)]
    [_ (gen-code-classcreate out sinfo '("java" "lang" "String") slit cenvs)]))

;==============================================================================================
;==== Array Generation
;==============================================================================================

;ARRAY CREATE
(define (gen-code-arraycreate out sinfo ty sz cenvs)
  (define lbl-sz-ok (symbol->string (gensym)))

  (push out "ebx")

  (nl out)
  (comment out "ARRAY CREATE")
  (gen-code-recurse out sinfo sz cenvs)
  (mov out "ebx" "eax")		;save the size of the array in ebx

  (comment out "Checking array size not negative")
  (cmp out "ebx" "0")
  (cjmp out "jge" lbl-sz-ok)
  (call out "__exception" "array size less than 0")
  (label out lbl-sz-ok)

  (addi out "eax" 2)		;add 1 to size so we can store the size
  (push out "ebx")
  (call out "__malloc")		;address will be in eax
  (pop out "ebx")
  (mov out "[eax+4]" "ebx")	;move the size of the array to the first element
  (nl out)

  (pop out "ebx"))

;ARRAY ACCESS
(define (gen-code-arrayaccess out sinfo rtnaddr left index cenvs) 
  (define lbl-sz-ok1 (symbol->string (gensym)))
  (define lbl-sz-ok2 (symbol->string (gensym)))
  (push out "ebx")

  (nl out)
  (comment out "ARRAY ACCESS")
  (gen-code-recurse out sinfo index cenvs)
  (mov out "ebx" "eax")			;ebx now holds the index offset
  (gen-code-recurse out sinfo left cenvs)	;eax now holds the array address

  (comment out "Checking index not negative")
  (cmp out "ebx" "0")
  (cjmp out "jge" lbl-sz-ok1)
  (call out "__exception")
  (label out lbl-sz-ok1)

  (comment out "Checking index not >= array size")
  (cmp out "ebx" "[eax+4]")
  (cjmp out "jl" lbl-sz-ok2)
  (call out "__exception")
  (label out lbl-sz-ok2)

  (add out "ebx" "2")
  (imul out "ebx" "4")
  (cond
    [rtnaddr (add out "eax" "ebx")]
    [else (mov out "eax" "[eax+ebx]")])

  (nl out)

  (pop out "ebx"))  

;==============================================================================================
;==== Field Access Generation
;==============================================================================================

(define (gen-code-fieldaccess out sinfo left field cenvs)
  (comment out "Fieldaccess")
  (gen-code-get-this out sinfo left cenvs))

;==============================================================================================
;==== "this" Generation
;==============================================================================================

(define (gen-code-this out sinfo type cenvs)
  (mov out "eax" "[ebp+8]")) 

(define (gen-code-get-this out sinfo t cenvs)
  (comment out "Getting \"this\"")
  (cond
    [(rtype? t) (mov out "ebx" "0")]				;static call to method, no this
    [(not (empty? t)) (gen-code-recurse out sinfo t cenvs)		;doing a method call on something which better be a class
                         (mov out "ebx" "eax")]			
    [else (mov out "ebx" "[ebp+8]")]))				;local method call, use current this

;==============================================================================================
;==== If / While / For Generation
;==============================================================================================

(define (gen-code-iff out sinfo test tru fls cenvs)
  (nl out)
  (comment out "IFF")
  (let  ([label-fls (symbol->string (gensym))]
	[label-end-of-if (symbol->string (gensym))])
    (comment out "Evaluating test")
    (gen-code-recurse out sinfo test cenvs)		;eval test, eax will contain 0 or 1 (false or true)
    (comment out "Done evaluating test")
    (nl out)
    (movi out "ecx" 1)				;mov 1 into ecx
    (cmp out "eax" "ecx")			;cmp test to ecx (true)
    (cjmp out "jne" label-fls)			;if test is eqaul to false then jump to the false code
    (nl out)
    (comment out "TRUE CODE")
    (gen-code-recurse out sinfo tru cenvs)		;else the test was false so run false code
    (jmp out label-end-of-if)			;done fls code so jump passed tru code
    (comment out "End of true code")		
    (nl out)
    (label out label-fls)
    (comment out "FALSE CODE")
    (gen-code-recurse out sinfo fls cenvs)		;tru code
    (comment out "End of false code") 
    (label out label-end-of-if)
    (nl out)))		;end of if statement

(define (gen-code-while out sinfo test body cenvs)
  (nl out)
  (comment out "WHILE")
  (let  ([label-cond (symbol->string (gensym))]
         [label-end (symbol->string (gensym))])
    (label out label-cond)
    (gen-code-recurse out sinfo test cenvs)
    (movi out "ecx" 1)				
    (cmp out "eax" "ecx")			
    (cjmp out "jne" label-end)			
    (gen-code-recurse out sinfo body cenvs)
    (jmp out label-cond)
    (label out label-end)
    (nl out)))

(define (gen-code-for out sinfo init clause update body cenvs)
  (let  ([label-cond (symbol->string (gensym))]
         [label-update (symbol->string (gensym))]
         [label-end (symbol->string (gensym))])

    (nl out)
    (comment out "FOR")
    (define new-sinfo (if [varassign? init] (gen-code-recurse out sinfo init cenvs) sinfo))
    (jmp out label-cond)
    (label out label-update)
    (gen-code-recurse out new-sinfo update cenvs)
    (label out label-cond)
    (gen-code-recurse out new-sinfo clause cenvs)
    (movi out "ecx" 1)				
    (cmp out "eax" "ecx")			
    (cjmp out "jne" label-end)			
    (gen-code-recurse out new-sinfo body cenvs)
    (jmp out label-update)
    (label out label-end)
    (reset-stack out (- (length (stackinfo-ldecls new-sinfo)) (length (stackinfo-ldecls sinfo))))
    (nl out)))

;==============================================================================================
;==== OTHER
;==============================================================================================

(define (gen-code-varuse out sinfo id cenvs)
  (comment out "varuse id: " id)
  (movf out "eax" "ebp" (get-ebp-offset id sinfo)))

(define (gen-code-literal out sinfo type val cenvs)
  (match type
    [(ptype 'int) (movi out "eax" val "lit int val " (number->string val))]
    [(ptype 'char) (movi out "eax" val "lit char val " (number->string val))]
    [(ptype 'byte) (movi out "eax" val "lit byte val " (number->string val))]
    [(ptype 'short) (movi out "eax" val "lit short val " (number->string val))]    
    [(ptype 'null) (movi out "eax" 0 "literal val null")]
    [(ptype 'boolean) (movi out "eax" (if val 1 0) "literal val bool")]
    [(rtype '("java" "lang" "String")) (gen-code-classcreate out sinfo '("java" "lang" "String") (literal empty type val) cenvs)]))

;;gen-code-cast: output stack-info type ast (listof codeenv) -> void
(define (gen-code-cast out sinfo c ex cenvs)
  ;(printf "gen-code-cast ~a~n" ex)
  (gen-code-recurse out sinfo ex cenvs)
  (match c
    [(ptype 'int)  (comment out "cast to int")]
    [(ptype 'byte) (display "movsx eax, al\t; cast to a byte\n" out)]
    [(ptype 'short) (display "movsx eax, ax\t; cast to a short\n" out)]
    [(ptype 'char) (display "movzx eax, ax\t; cast to a char\n" out)]
    [(rtype '("java" "lang" "Object")) (comment out "cast to object")]
    [(rtype name) (let ([cenv (find-codeenv name cenvs)]
			[fail-label (gensym "castable-fail")]
			[success-label (gensym "castable-success")])
		  (cond
			[(codeenv-class? cenv)
	      			(push out "eax")
		  		(push out "ebx")
		  		(gen-get-class-id out "eax")
		  		;;get id list
		  		(let ([id-list (codeenv-casts cenv)])
			  		(gen-check-if-castable out id-list "eax" "ebx" fail-label success-label))
				(label out fail-label)
				(call out "__exception" "Bad Cast")
				(label out success-label "Valid cast")
		  		(pop out "ebx")
		  		(pop out "eax")]
			[else (error 'cast-rtype-interface "unimplemented")]))]
    [(atype type) (error 'cast-atype "unimplemented")]
    ))
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

(define (stackinfo-add-ldecl sinfo id)
  (define ldecls (stackinfo-ldecls sinfo))
  (define ebpoff (stackinfo-ebpoff sinfo))
  (stackinfo (stackinfo-mdecls sinfo)
             (cons (list id (string-append "-" (number->string ebpoff))) ldecls)
             (stackinfo-sdecls sinfo)
             (+ ebpoff WORD)))

(define (stackinfo-add-sdecl sinfo ex)
  (cond
    [(literal? ex) (equal? (literal-type ex) (rtype '("java" "lang" "String")))
     (stackinfo (stackinfo-mdecls sinfo)
                (stackinfo-ldecls sinfo)
                (cons (list (literal-value ex) (second (first (stackinfo-ldecls sinfo)))) 
                      (stackinfo-sdecls sinfo))
                (stackinfo-ebpoff sinfo))]
    [else sinfo]))


;;the register points to the object. The caller preserves the register. 
(define (gen-get-class-id out register)
	(movf out register register "0" "Getting static class info")
	(movf out register register "0" "Getting the class number"))

(define (gen-check-if-castable out id-list register check-register fail-label success-label)
	(cond
		[(empty? id-list) 
			(jmp out fail-label)]
		[else
			(movi out check-register (first id-list))
			(cmp check-register register)
			(cjmp out "je" success-label)
			(gen-check-if-castable out (rest id-list) register fail-label success-label)])) 
			

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
  (display "extern __exception\n" out)
  (display "extern __malloc\n" out)
  (display "extern NATIVEjava.io.OutputStream.nativeWrite\n" out))









