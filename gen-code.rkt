#lang racket

(require "class-info.rkt")
(require "ast-tree.rkt")
(require "types.rkt")

(require "gen-headers.rkt")
(require "environments.rkt")
(require "generation-structures.rkt")
(require "mangle-names.rkt")

(provide gen-code)

(struct stackinfo (mdecls ldecls sdecls strdecls ebpoff))

(define WORD 4)
(define ARRAY-SZ-LOC "1")	;offset in bytes to size of array
(define ARRAY-HEADER-SZ "2")	;size of header in bytes
(define empty-stackinfo (stackinfo empty empty empty empty 0))

(define (get-outfile cinfo)
  (string-append "output/" (foldr string-append "" (info-name cinfo)) ".s"))

(define (gen-code cinfo cenvs)
  (define out (open-output-file (get-outfile cinfo) #:exists 'replace))
  (define cenv (find-codeenv (info-name cinfo) cenvs))
  (define sdecls (get-static-decls (codeenv-vars cenv)))
  (define entry-label (mangle-names (find-codemeth (funt "test" empty) (codeenv-methods cenv))))
  (gen-static out cenv)
  (display "\n\n\nsection .text\n\n" out)
  (gen-runtime-externs out)
  (gen-debug-print-eax out)

  (display "\n;================================================\n" out)
  (display ";====== Class code\n" out)
  (display ";================================================\n\n" out)

  (for-each(lambda (x)
             (for-each (curryr display out) (list  (mangle-names x) ":\t; Method Def - " (funt-id (codemeth-id x)) "\n"))
             (match (codemeth-def x)
               [(constructor env sp (methoddecl _ id params) bd) (gen-code-constructor out sdecls (codeenv-parent cenv) params bd cenvs)]
               [(method _ _ '(static) (ptype 'int) (methoddecl _ "test" `()) bd) (gen-code-start-method out sdecls entry-label bd cenvs)]
               [(method env sp md ty (methoddecl _ id params) bd) (gen-code-method out sdecls params bd cenvs)])) 
    (filter codemeth-ref? (codeenv-methods cenv))))


(define (gen-code-recurse out sinfo t cenvs)
  (match t
    [(varassign env id ex) (gen-code-varassign out sinfo id ex cenvs)]
    [(binop env op ls rs) (gen-code-binop out sinfo op ls rs cenvs)]
    [(unop env op rs) (gen-code-unop out sinfo op rs cenvs)]
    [(cast env c ex) (gen-code-cast out sinfo c ex cenvs)]
    [(arraycreate env ty sz) (gen-code-arraycreate out sinfo ty sz cenvs)]
    [(classcreate env (rtype cls) params) (gen-code-classcreate out sinfo cls params cenvs)]
    [(fieldaccess env left field) (gen-code-fieldaccess out sinfo left field cenvs)]
    [(arrayaccess env left index) (gen-code-arrayaccess out sinfo #f left index cenvs)]
    [(while env test body) (gen-code-while out sinfo test body cenvs)]
    [(methodcall env left id args) (gen-code-methodcall out sinfo left id args cenvs)]
    [(iff env test tru fls) (gen-code-iff out sinfo test tru fls cenvs)]
    [(return env expr) (gen-code-return out sinfo expr cenvs)]
    [(for env init clause update body) (gen-code-for out sinfo init clause update body cenvs)]
    [(varuse _ id) (gen-code-varuse out sinfo id cenvs)]
    [(this _ type) (gen-code-this out)]
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
  (printf "gen-code-varassign: ~a~n" id)
  (gen-code-recurse out sinfo ex cenvs)	;puts result in eax
  (cond
    [(vdecl? id)  (push out "eax" "declaring " (vdecl-id id))
                  (stackinfo-add-sdecl (stackinfo-add-ldecl sinfo (vdecl-id id)) ex)]
    [(varuse? id) (mov out (string-append "[" (get-var-mem-loc (varuse-id id) sinfo) "]") "eax" "assigning " (varuse-id id))
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

;CONSTRUCTOR
(define (gen-code-constructor out sdecls parent params bd cenvs)
  ;(if [empty? parent] (printf "") (call out (constr-label parent params)))
  (gen-code-method out sdecls params bd cenvs))

;CLASS CREATE
(define (gen-code-classcreate out sinfo cls args cenvs)
  (comment out "CLASS CREATE " (foldr string-append "" cls))
  (push out "ebx")

  (cond 
    [(jlstring-slit? cls args) (stringlit->chararray out sinfo (first args))
                               (push out "eax" "push char array on stack")]
    [else (push-method-args out sinfo args cenvs)])	;push args onto stack
  
  (malloc out (codeenv-size (find-codeenv cls cenvs)))
  (push out "eax")	;push "this" onto stack
  (comment out "TODO: call new class constructor here")
  (pop out "eax")			;pop "this" off stack and return it

  (reset-stack out (length args))	;pop this and args off stack
  (pop out "ebx")
  (comment out "END CLASS CREATE"))

;Take string literal and create a char array, eax will contain address of char array
(define (stringlit->chararray out sinfo slit)
  (define c-str (rest (reverse (rest (reverse (string->list (literal-value slit)))))))	;remove string quotes (ie '(" a b c ") -> '(a b c))
  (comment out "Converting String to char[]")
  (arraycreate-sz out sinfo (ptype 'char) (length c-str))
  (comment out "Adding chars to char[]")
  (copy-to-chararray out c-str 0))
  
(define (copy-to-chararray out c-str i)
  (cond
    [(empty? c-str) (printf "")]
    [else (movi out "ecx" (char->integer (first c-str)))
          (mov out (string-append "[eax+" (number->string (* WORD (+ i (string->number ARRAY-HEADER-SZ)))) "]") "ecx")
          (copy-to-chararray out (rest c-str) (+ i 1))]))

(define (jlstring-slit? cls args)
  (and (equal? cls '("java" "lang" "String"))
       (equal? 1 (length args))
       (stringlit? (first args))))

;==============================================================================================
;==== Method Generation
;==============================================================================================

;ENTRY POINT
(define (gen-code-start-method out sdecls entry-label bd cenvs)
  (gen-code-method out sdecls empty bd cenvs)
  (display "global _start\n" out)
  (display "_start:\n" out)

  (comment out "@@@@@@@@@@@@ Initialize Static variables! @@@@@@@")
  (gen-initialize-static-fields out bd cenvs)


  (comment out "@@@@@@@@@@@ Done static initialization! @@@@@@@")
  (call out entry-label)
  (gen-debug-print out)
  (display "mov eax, 1\n" out)
  (display "int 0x80\n" out)
  (display "int 3\n" out))

;METHOD DECLARATION
(define (gen-code-method out sdecls params bd cenvs)
  (printf "SDECLS: ~a~n" sdecls)
  (define sinfo (stackinfo (get-method-arg-decls (reverse params) 12) empty sdecls empty 4))
  (comment out "method prolog")
  (push out "ebp")			;save frame pointer
  (mov out "ebp" "esp")			;set new frame pointer to stack pointer
  (nl out)

  (comment out "method body")
  (gen-code-recurse out sinfo bd cenvs)	;gen methods code
  (nl out)

  (comment out "method epilog")
  (pop out "ebp")
  (ret out)
  (nl out))			;ret from method

;METHOD CALL
(define (gen-code-methodcall out sinfo left id args cenvs)
  (push out "ebx")
  (gen-code-get-this out sinfo left cenvs)	;puts "this" in ebx

  (comment out "Pushing method args on stack")
  (push-method-args out sinfo args cenvs)	;push all method args onto stack
  (push out "ebx" "this") 			;push the addr of "this"
  (call out id)					;TODO: call right label
  (reset-stack out (+ 1 (length args)))		;"pop" off all the args from the stack
  (pop out "ebx"))

;METHOD RETURN
(define (gen-code-return out sinfo expr cenvs)
  (gen-code-recurse out sinfo expr cenvs)
  (reset-stack out (length (stackinfo-ldecls sinfo)))
  (pop out "ebp")
  (ret out))			;ret from method

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

(define (get-static-decls codevars)
  (map (lambda(cvar) (list (codevar-id cvar) (mangle-names cvar))) (filter codevar-static? codevars)))
  

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
  (printf "gen-code-binop: ~a ~a ~a~n" ls op rs)
  (push out "ebx" "saving")		;save ebx (cause we gonna use it)
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
  (let ([ltrue (symbol->string (gensym "true"))]	
        [lend (symbol->string (gensym "end"))])
    (cmp out "eax" "ebx")				;first compare the 2 operations
    (match op					;then put the correct comditional jmp to a tru label 
      ['eqeq (cjmp out "je" ltrue)]
      ['noteq (cjmp out "jne" ltrue)]
      ['gt (cjmp out "jg" ltrue)]
      ['lt (cjmp out "jl" ltrue)]
      ['gteq (cjmp out "jge" ltrue)]
      ['lteq (cjmp out "jle" ltrue)])
    (movi out "eax" 0)				;if the condition was false set eax to 0
    (jmp out lend)
    (label out ltrue)				;and skip passed true code
    (movi out "eax" 1)				;if condition was true set eax to 1
    (label out lend)))

;Helper - LOGICAL
(define (gen-code-logical out sinfo op ls rs cenvs)
  (let ([lend (symbol->string (gensym "end"))])
    (gen-code-recurse out sinfo ls cenvs)
    (cmp out "eax" "1")
    (match op
      ['barbar (cjmp out "je" lend)
               (gen-code-recurse out sinfo rs cenvs)
               (label out lend)]
      ['ampamp (cjmp out "jne" lend)
               (gen-code-recurse out sinfo rs cenvs)
               (label out lend)])))

;;helper - INSTANCEOF
;;this code is mostly copied from the gen-code-cast function
(define (gen-code-instanceof out sinfo ls rs cenvs)
	(comment out "Getting lhs of instanceof")
	(gen-code-recurse out sinfo ls cenvs)
	(comment out "We now have lhs of instanceof")
	(let*
		([fail-label (symbol->string (gensym "instanceoffail"))]
		 [success-label (symbol->string (gensym "instanceofsuccess"))]
		 [end-label (symbol->string (gensym "instanceofend"))]
  		 [name (rtype-type rs)]
		 [cenv (find-codeenv name cenvs)] )
		
		(cond
			[(codeenv-class? cenv)
				(movi out "ebx" 0)
				(cmp out "eax" "ebx")
				(cjmp out "je" fail-label "Null literal is automatically false")
		  		(gen-get-class-id out "eax")
		  		;;get id list
		  		(let ([id-list (codeenv-casts cenv)])
					(printf "id-list for : ~a ~a~n" name id-list)
			  		(gen-check-if-castable out id-list "eax" "ebx" success-label))
				(label out fail-label)
				(movi out "eax" 0)
				(jmp out end-label)
				(label out success-label)
				(movi out "eax" 0)
				(label out end-label)]
			[else (error 'cast-rtype-interface "unimplemented")])))

(define (stringlit? t)
  (and (literal? t) (equal? (literal-type t) (rtype '("java" "lang" "String")))))



(define (gen-code-stringlit out sinfo slit cenvs)
  (match (assoc (literal-value slit) (stackinfo-strdecls sinfo))
    [(list key value) (movf out "eax" "ebp" value)]
    [_ (gen-code-classcreate out sinfo '("java" "lang" "String") (list slit) cenvs)]))

;==============================================================================================
;==== Array Generation
;==============================================================================================

(define (arraycreate-sz out sinfo ty size)	;used by us when we know the size of the array
  (push out "ebx")
  (movi out "eax" size)	;used by malloc, 2 is added to this register in gen-arraycreate-code
  (movi out "ebx" size)	;ebx is placed into the array as the array size in gen-arraycreate-code
  (gen-arraycreate-code out)
  (pop out "ebx")) 
  

;ARRAY CREATE
(define (gen-code-arraycreate out sinfo ty sz cenvs)
  (define no-exception (symbol->string (gensym "no-exception")))

  (push out "ebx")

  (comment out "ARRAY CREATE")
  (gen-code-recurse out sinfo sz cenvs)
  (mov out "ebx" "eax")		;save the size of the array in ebx

  (comment out "Checking array size not negative")
  (cmp out "ebx" "0")
  (cjmp out "jge" no-exception)
  (call out "__exception" "array size less than 0")
  (label out no-exception)

  (gen-arraycreate-code out)

  (pop out "ebx"))

(define (gen-arraycreate-code out)
  (add out "eax" ARRAY-HEADER-SZ)	;add 2 to size so we can store the size
  (imul out "eax" "4")
  (push out "ebx")
  (call out "__malloc")		;address will be in eax
  (pop out "ebx")
  (mov out "[eax+4]" "ebx"))	;move the size of the array to the first element

;ARRAY ACCESS
(define (gen-code-arrayaccess out sinfo rtnaddr left index cenvs) 
  (define lbl-sz-ok1 (symbol->string (gensym "noexception1")))
  (define lbl-sz-ok2 (symbol->string (gensym "noexception2")))
  (push out "ebx")

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
  (cmp out "ebx" (string-append "[eax+4*"ARRAY-SZ-LOC"]"))
  (cjmp out "jl" lbl-sz-ok2)
  (call out "__exception")
  (label out lbl-sz-ok2)

  (gen-arrayaccess-code out rtnaddr)

  (pop out "ebx"))  

(define (gen-arrayaccess-index out index)
  (movi out "ebx" index)
  (gen-arrayaccess-code out #t))

(define (gen-arrayaccess-code out rtnaddr)
  (add out "ebx" ARRAY-HEADER-SZ)
  (imul out "ebx" "4")
  (cond
    [rtnaddr (add out "eax" "ebx")]
    [else (mov out "eax" "[eax+ebx]")]))
  

;==============================================================================================
;==== Field Access Generation
;==============================================================================================

(define (gen-code-fieldaccess out sinfo left field cenvs)
  (comment out "Fieldaccess")
  (gen-code-get-this out sinfo left cenvs))

;==============================================================================================
;==== "this" Generation
;==============================================================================================

(define (gen-code-this out)
  (mov out "eax" "[ebp+8]")) 

(define (gen-code-get-this out sinfo t cenvs)
  (define non-null (symbol->string (gensym "non-null")))
  (comment out "Getting \"this\"")

  (cond
    [(rtype? t) (mov out "ebx" "0")		;static call to method, no this
                (jmp out non-null)]		;since static call, no need to check null
    [(not (empty? t)) (gen-code-recurse out sinfo t cenvs)	;doing a method call on something which better be a class
                      (mov out "ebx" "eax")]			
    [else (mov out "ebx" "[ebp+8]")])		;t is empty, use local this
  
  (cmp "ebx" "0")
  (cjmp out "jne" non-null)
  (call out "__exception")
  (label out non-null))				;local method call, use current this

;==============================================================================================
;==== If / While / For Generation
;==============================================================================================

(define (gen-code-iff out sinfo test tru fls cenvs)
  (comment out "IFF")
  (let  ([label-fls (symbol->string (gensym "iffalse"))]
	[label-end-of-if (symbol->string (gensym "ifend"))])
    (comment out "Evaluating test")
    (gen-code-recurse out sinfo test cenvs)		;eval test, eax will contain 0 or 1 (false or true)
    (comment out "Done evaluating test")

    (movi out "ecx" 1)				;mov 1 into ecx
    (cmp out "eax" "ecx")			;cmp test to ecx (true)
    (cjmp out "jne" label-fls)			;if test is eqaul to false then jump to the false code

    (comment out "TRUE CODE")
    (gen-code-recurse out sinfo tru cenvs)		;else the test was false so run false code
    (jmp out label-end-of-if)			;done fls code so jump passed tru code
    (comment out "End of true code")		

    (label out label-fls)
    (comment out "FALSE CODE")
    (gen-code-recurse out sinfo fls cenvs)		;tru code
    (comment out "End of false code") 
    (label out label-end-of-if)))		;end of if statement

(define (gen-code-while out sinfo test body cenvs)
  (let  ([label-cond (symbol->string (gensym "while-check-condition"))]
         [label-end (symbol->string (gensym "while-end"))])
    (label out label-cond)
    (gen-code-recurse out sinfo test cenvs)
    (movi out "ecx" 1)				
    (cmp out "eax" "ecx")			
    (cjmp out "jne" label-end)			
    (gen-code-recurse out sinfo body cenvs)
    (jmp out label-cond)
    (label out label-end)))

(define (gen-code-for out sinfo init clause update body cenvs)
  (let  ([label-cond (symbol->string (gensym "for-check-condition"))]
         [label-update (symbol->string (gensym "for-update"))]
         [label-end (symbol->string (gensym "for-end"))])
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
    (reset-stack out (- (length (stackinfo-ldecls new-sinfo)) (length (stackinfo-ldecls sinfo))))))

;==============================================================================================
;==== OTHER
;==============================================================================================

(define (gen-code-varuse out sinfo id cenvs)
  (mov out "eax" (string-append "[" (get-var-mem-loc id sinfo) "]")))

(define (gen-code-literal out sinfo type val cenvs)
  (match type
    [(ptype 'int) (movi out "eax" val "lit int val " (number->string val))]
    [(ptype 'char) (movi out "eax" val "lit char val " (number->string val))]
    [(ptype 'byte) (movi out "eax" val "lit byte val " (number->string val))]
    [(ptype 'short) (movi out "eax" val "lit short val " (number->string val))]    
    [(ptype 'null) (movi out "eax" 0 "literal val null")]
    [(ptype 'boolean) (movi out "eax" (if val 1 0) "literal val bool")]
    [(rtype '("java" "lang" "String")) (gen-code-classcreate out sinfo '("java" "lang" "String") (list (literal empty type val)) cenvs)]))

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
			[fail-label (symbol->string (gensym "castablefail"))]
			[success-label (symbol->string (gensym "castablesuccess"))])
		  (cond
			[(codeenv-class? cenv)
	      			(push out "eax")
		  		(push out "ebx")
		  		(gen-get-class-id out "eax")
		  		;;get id list
		  		(let ([id-list (codeenv-casts cenv)])
			  		(gen-check-if-castable out id-list "eax" "ebx" success-label))
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

(define (get-var-mem-loc id sinfo)
  (define mdecl (assoc id (stackinfo-mdecls sinfo)))
  (define ldecl (assoc id (stackinfo-ldecls sinfo)))
  (define sdecl (assoc id (stackinfo-sdecls sinfo)))
  (cond
    [(and (list? mdecl) (list? ldecl)) (error "We have clashing decls in m and l")]
    [(list? mdecl) (string-append "ebp" (second mdecl))] ;method param, get off stack
    [(list? ldecl) (string-append "ebp" (second ldecl))] ;local variable, get off stack
    [(list? sdecl) (second sdecl)] ;static variable, get using label
    [else (error "Could find a declaration for a variable? No test should be like that.")]))

(define (stackinfo-add-ldecl sinfo id)
  (define ldecls (stackinfo-ldecls sinfo))
  (define ebpoff (stackinfo-ebpoff sinfo))
  (stackinfo (stackinfo-mdecls sinfo)
             (cons (list id (string-append "-" (number->string ebpoff))) ldecls)
             (stackinfo-sdecls sinfo)
             (stackinfo-strdecls sinfo)
             (+ ebpoff WORD)))

(define (stackinfo-add-sdecl sinfo ex)
  (cond
    [(literal? ex) (equal? (literal-type ex) (rtype '("java" "lang" "String")))
     (stackinfo (stackinfo-mdecls sinfo)
                (stackinfo-ldecls sinfo)
                (stackinfo-sdecls sinfo)
                (cons (list (literal-value ex) (second (first (stackinfo-ldecls sinfo)))) 
                      (stackinfo-strdecls sinfo))
                (stackinfo-ebpoff sinfo))]
    [else sinfo]))


;;the register points to the object. The caller preserves the register. 
(define (gen-get-class-id out register)
	(movf out register register "" "Getting static class info")
	(movf out register register "" "Getting the class number"))

(define (gen-check-if-castable out id-list register check-register success-label)
	(cond
		[(empty? id-list) 
			(nop out "failure; the next instruction should be the failure code")]
		[else
			(movi out check-register (first id-list))
			(cmp out check-register register)
			(cjmp out "je" success-label)
			(gen-check-if-castable out (rest id-list) register check-register success-label)])) 
			

(define (gen-initialize-static-fields out bd cenvs)
	(printf "gen-initialize-static-fields: ~a~n" cenvs)
	(define (gen-initialize-static-fields-class cenv)
		(map (lambda (cvar)
				(gen-code-recurse out empty (codevar-val cvar) cenvs) 
				(mov out (string-append "[" (mangle-names cvar)  "]") "eax"))
			 (reverse (filter (lambda (x) (and (not (empty? (codevar-val x))) (codevar-static? x))) (codeenv-vars cenv)))))
	(map gen-initialize-static-fields-class cenvs)) 	


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
  (display (foldr string-append "" (append (cons "\t;" m) (list "\n"))) out))

(define (cmt out lm)
  (display (foldr string-append "" (append (cons "\t;" lm) (list "\n"))) out))

(define (push out reg . comment)
  (display (string-append "\tpush " reg) out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

(define (pop out reg . comment)
  (display (string-append "\tpop " reg) out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

(define (label out l . comment)
  (display (string-append "    " l ":") out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

(define (call out label . comment)
  (display (string-append "\tcall " label) out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

(define (mov out reg1 reg2 . comment)
  (display (string-append "\tmov " reg1 "," reg2 ) out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

(define (movi out reg i . comment)
  (display (string-append "\tmov " reg "," (number->string i)) out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

;mov from mem (like lw)
(define (movf out dst src off . comment)
  (display (string-append "\tmov " dst ",["src off "]") out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

;mov to mem (like sw)
(define (movt out dst src off . comment)
  (display (string-append "\tmov [" dst off "]," src) out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

(define (sub out reg1 reg2 . comment)
  (display (string-append "\tsub " reg1 "," reg2) out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

(define (add out reg1 reg2 . comment)
  (display (string-append "\tadd " reg1 "," reg2) out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

(define (addi out reg i . comment)
  (display (string-append "\tadd " reg "," (number->string i)) out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

(define (imul out reg1 reg2 . comment)
  (display (string-append "\timul " reg1 "," reg2) out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

(define (cmp out reg1 reg2 . comment)
  (display (string-append "\tcmp " reg1 "," reg2) out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

(define (jmp out label . comment)
  (display (string-append "\tjmp " label) out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

(define (cjmp out cj label . comment)
  (display (string-append "\t" cj " " label) out)
  (if [> (length comment) 0] (cmt out comment) (display "\n" out)))

(define (ret out)
  (display "\tret\t;RETURN\n" out))

(define (nop out . comment)
 (display "\tnop" out)
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
  (display ";================================================\n" out)
  (display ";====== Debug code\n" out)
  (display ";================================================\n\n" out)

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

(define (gen-runtime-externs out)
  (display "extern __exception\n" out)
  (display "extern __malloc\n" out)
  (display ";v Delete this, its used for debugging v\n" out)
  (display "extern NATIVEjava.io.OutputStream.nativeWrite\n\n" out))









