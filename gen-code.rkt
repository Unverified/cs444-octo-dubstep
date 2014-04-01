#lang racket

(require "class-info.rkt")
(require "ast-tree.rkt")
(require "types.rkt")

(require "gen-headers.rkt")
(require "environments.rkt")
(require "generation-structures.rkt")
(require "mangle-names.rkt")

(provide gen-all-code)

(struct stackinfo (mdecls ldecls strdecls ebpoff))

(define WORD 4)
(define ARRAY-SZ-LOC "2")	;offset in bytes to size of array
(define ARRAY-HEADER-SZ "3")	;size of header in bytes
(define ARRAY-LABEL "__ARRAY")

(define (get-outfile env)
  (string-append "output/" (foldr string-append "" (codeenv-name env)) ".s"))

(define (gen-all-code cenvs)
  (define out (open-output-file (string->path "output/-start.s") #:exists 'replace))
  (define-values (classes interfaces) (partition codeenv-class? cenvs))
  (define all-labels (remove-duplicates (map mangle-names (append (filter (lambda (x) (and (codevar-static? x) (codevar-ref? x))) (append-map codeenv-vars classes))
                                                                  (filter codemeth-ref? (append-map codeenv-methods classes))
                                                                  cenvs))))
  
  (gen-code-start out all-labels cenvs)
  
  (for-each gen-interface interfaces)
  (for-each (curry gen-code (cons ARRAY-LABEL all-labels) cenvs) classes))

(define (gen-interface cenv)
  (define out (open-output-file (get-outfile cenv) #:exists 'replace))
  (display (string-append "global " (mangle-names cenv) "\n\n") out)
  (display (string-append (mangle-names cenv) ":\n") out)
  (for-each (curryr display out) (list "\tdd " (codeenv-guid cenv) "\t; set up the guid\n")))

(define (gen-code all-labels cenvs cenv)
  (define out (open-output-file (get-outfile cenv) #:exists 'replace))
  (gen-static out all-labels cenv)
  (display "\n\n\nsection .text\n\n" out)
  (gen-runtime-externs out)
  
  (display "\n;================================================\n" out)
  (display ";====== Class code\n" out)
  (display ";================================================\n\n" out)
  
  (for-each (lambda (x)
              (for-each (curryr display out) (list  (mangle-names x) ":\t; Method Def - " (funt-id (codemeth-id x)) "\n"))
              (match (codemeth-def x)
                [(constructor env sp (methoddecl _ id params) bd) (gen-code-constructor out cenv params bd cenvs)]
                [(method env sp md ty (methoddecl _ id params) bd) (gen-code-method out cenv params bd cenvs)]))
            (filter codemeth-ref? (codeenv-methods cenv))))

(define (gen-code-recurse out cenv sinfo t cenvs)
  (match t
    [(varassign env id ex) (gen-code-varassign out cenv sinfo id ex cenvs)]
    [(binop env op ls rs) (gen-code-binop out cenv sinfo op ls rs cenvs)]
    [(unop env op rs) (gen-code-unop out cenv sinfo op rs cenvs)]
    [(cast env c ex) (gen-code-cast out cenv sinfo c ex cenvs)]
    [(arraycreate env ty sz) (gen-code-arraycreate out cenv sinfo ty sz cenvs)]
    [(classcreate (rtype typ) cls params) (gen-code-classcreate out cenv sinfo cls typ params cenvs)]
    [(fieldaccess lty left field) (gen-code-fieldaccess out cenv sinfo #f left field cenvs)]
    [(arrayaccess env left index) (gen-code-arrayaccess out cenv sinfo #f left index cenvs)]
    [(while env test body) (gen-code-while out cenv sinfo test body cenvs)]
    [(methodcall env left id args) (gen-code-methodcall out cenv sinfo left id args cenvs)]
    [(iff env test tru fls) (gen-code-iff out cenv sinfo test tru fls cenvs)]
    [(return env expr) (gen-code-return out cenv sinfo expr cenvs)]
    [(for env init clause update body) (gen-code-for out cenv sinfo init clause update body cenvs)]
    [(varuse _ id) (gen-code-varuse-read out cenv sinfo id cenvs)]
    [(this _ type) (gen-code-this out)]
    [(literal _ type val) (gen-code-literal out cenv sinfo type val cenvs)]
    [(block env id statements)  (gen-code-block out cenv sinfo id statements cenvs) ]
    [`() (comment out "EMPTY STATEMENT")]))

;==============================================================================================
;==== Block Generation
;==============================================================================================

(define (gen-code-block out cenv sinfo id statements cenvs)
  (define (gen-code-block-helper sinfo statements)
    (cond
      [(empty? statements) sinfo] ;TODO: pop local vars off stack
      [(varassign? (first statements)) (gen-code-block-helper (gen-code-recurse out cenv sinfo (first statements) cenvs) (rest statements))]
      [else (gen-code-recurse out cenv sinfo (first statements) cenvs)
            (gen-code-block-helper sinfo (rest statements))]))
  
  (define bsinfo (gen-code-block-helper sinfo statements))
  (reset-stack out (- (length (stackinfo-ldecls bsinfo)) (length (stackinfo-ldecls sinfo)))))

;==============================================================================================
;==== Variable Assignment Generation
;==============================================================================================

(define (gen-code-varassign out cenv sinfo lhs ex cenvs)
  (gen-code-recurse out cenv sinfo ex cenvs)	;puts result in eax
  (match lhs
    [(vdecl _ _ _ _ id)  (push out "eax" "declaring " id)
                         (stackinfo-add-strdecl (stackinfo-add-ldecl sinfo id) id ex)]
    [(varuse _ id) (gen-code-varuse-write out cenv sinfo id)
                   (stackinfo-add-strdecl (stackinfo-rmv-modified-strdecl sinfo id) id ex)] ;need to remove any string decls if they are modified, but add them back in if they were modified to another stringlit
    [(arrayaccess _ left id) (push out "ebx" "saving")
                             (mov out "ebx" "eax")
                             (gen-code-arrayaccess out cenv sinfo #t left id cenvs)
                             (mov out "[eax]" "ebx")
                             (pop out "ebx")
                             sinfo]
    [(fieldaccess _ left field) (push out "ebx" "saving")
                                (mov out "ebx" "eax")
                                (gen-code-fieldaccess out cenv sinfo #t left field cenvs)
                                (mov out "[eax]" "ebx")
                                (pop out "ebx")
                                sinfo
                                
                                ]))

;==============================================================================================
;==== Class Generation
;==============================================================================================

;CONSTRUCTOR
(define (gen-code-constructor out cenv params bd cenvs)
  (let ([parent (codeenv-parent cenv)]
        [member-vars (filter-not codevar-static? (codeenv-vars cenv))]
        [sinfo (stackinfo (get-method-arg-decls (reverse params) 12) empty empty 4)])
    (push out "ebp")			
    (mov out "ebp" "esp")

    (push out "ebx" "saving")	
    (load-membervars-into-this out cenv member-vars cenvs)
    (pop out "ebx" "restoring")

    (if (empty? parent) (printf "") (call out (constr-label parent empty)))
    (gen-code-recurse out cenv sinfo bd cenvs)
    (pop out "ebp")
    (ret out)))

;CLASS CREATE
(define (gen-code-classcreate out cenv sinfo cls rty args cenvs)
  (let* ([newcenv (find-codeenv rty cenvs)]
         [mcvar (find-codemeth cls (codeenv-methods newcenv))]
         [csize (codeenv-size newcenv)])

    (comment out "CLASS CREATE " (foldr string-append "" rty))
    (push out "ebx")

    (cond 
      [(jlstring-slit? rty args) (stringlit->chararray out cenv sinfo (first args))
                                 (push out "eax" "push char array on stack")]
      [else (push-method-args out cenv sinfo args cenvs)])	;push args onto stack
  
    (malloc out csize)
    (mov out "ecx" (mangle-names newcenv))
    (mov out "[eax]" "ecx")
    (push out "eax")			;push "this" onto stack
    (call out (mangle-names mcvar))
    (pop out "eax")			;pop "this" off stack and return it

    (reset-stack out (length args))	;pop this and args off stack
    (pop out "ebx")
    (comment out "END CLASS CREATE")))

;Take string literal and create a char array, eax will contain address of char array
(define (stringlit->chararray out cenv sinfo slit)
  (define c-str (rest (reverse (rest (reverse (string->list (literal-value slit)))))))	;remove string quotes (ie '(" a b c ") -> '(a b c))
  (comment out "Converting String to char[]")
  (arraycreate-sz out cenv sinfo (ptype 'char) (length c-str))
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


;(struct codevar (id ref? static? tag val) #:transparent)
(define (load-membervars-into-this out cenv member-vars cenvs)
  (define sinfo (stackinfo empty empty empty 4))
  (mov out "ebx" "[ebp+8]")
  (map (lambda(mvar) (gen-code-recurse out cenv sinfo (codevar-val mvar) cenvs)
                     (mov out (string-append "[ebx+"(number->string (codevar-tag mvar)) "]") "eax")) 
         member-vars))

;==============================================================================================
;==== Method Generation
;==============================================================================================

;ENTRY POINT
(define (gen-code-start out labels cenvs)
  (let ([entry-label (mangle-names (find-codemeth (funt "test" empty) (codeenv-methods (first cenvs))))])  
    (display "global _start\n" out)
    (display (string-append "global " ARRAY-LABEL "\n\n") out)
      
    (for-each (lambda (x) (display (string-append "extern " x "\n") out)) labels)
    (display "extern NATIVEjava.io.OutputStream.nativeWrite\n" out) 
    (gen-runtime-externs out)
    
    (display "section .data\n\n" out)
    (display (string-append ARRAY-LABEL ":\n") out)
    (display (string-append "\tdd " (number->string (name->id "array")) "\n") out)
    
    (for-each (lambda (x) (display (string-append "\tdd " (mangle-names x) "\n") out))
              (filter-not (compose1 (curry equal? "") funt-id codemeth-id) 
                          (reverse (codeenv-methods (find-codeenv '("java" "lang" "Object") cenvs)))))
    
    (display "section .text\n\n" out)
    (gen-debug-print-eax out)
    
    (comment out "@@@@@@@@@@@@@ ENTRY POINT @@@@@@@@@@@@@")
    (display "_start:\n" out)
    
    (comment out "@@@@@@@@@@@@ Initialize Static variables! @@@@@@@")
    (gen-initialize-static-fields out cenvs)
    
    (comment out "@@@@@@@@@@@ Done static initialization! @@@@@@@")
    (call out entry-label)
    (mov out "ebx" "eax")
    (display "mov eax, 1\n" out)
    (display "int 0x80\n" out)
    (display "int 3\n" out)))

;METHOD DECLARATION
(define (gen-code-method out cenv params bd cenvs)
  (define sinfo (stackinfo (get-method-arg-decls (reverse params) 12) empty empty 4))
  (comment out "method prolog")
  (push out "ebp")			
  (mov out "ebp" "esp")			
  (nl out)
  
  (comment out "method body")
  (gen-code-recurse out cenv sinfo bd cenvs)	;gen methods code
  (nl out)
  
  (comment out "method epilog")
  (pop out "ebp")
  (ret out)
  (nl out))			;ret from method

;METHOD CALL
(define (gen-code-methodcall out cenv sinfo left mfunt args cenvs)
  (define mcvar (find-codemeth mfunt (codeenv-methods (find-codeenv (rtype-type (get-left-type left)) cenvs))))

  (push out "ebx")
  (gen-code-get-this out cenv sinfo left cenvs)	;puts "this" in ebx
  
  (comment out "Pushing method args on stack")
  (push-method-args out cenv sinfo args cenvs)	;push all method args onto stack
  (push out "ebx" "this") 			;push the addr of "this"
  (call out (mangle-names mcvar))					
  (reset-stack out (+ 1 (length args)))		;"pop" off all the args from the stack
  (pop out "ebx"))


;METHOD RETURN
(define (gen-code-return out cenv sinfo expr cenvs)
  (gen-code-recurse out cenv sinfo expr cenvs)
  (reset-stack out (length (stackinfo-ldecls sinfo)))
  (pop out "ebp")
  (ret out))			;ret from method

;------- Method Helpers -------

(define (reset-stack out n)
  (cond
    [(> n 0) (addi out "esp" (* WORD n))]
    [else (printf "")]))

(define (push-method-args out cenv sinfo args cenvs)
  (cond
    [(empty? args) (printf "")]
    [else (gen-code-recurse out cenv sinfo (first args) cenvs)
          (push out "eax")
          (push-method-args out cenv sinfo (rest args) cenvs)]))

(define (get-method-arg-decls params ebpoff)
  (cond
    [(empty? params) empty]
    [else (cons (list (parameter-id (first params)) (string-append "+" (number->string ebpoff))) 
                (get-method-arg-decls (rest params) (+ WORD ebpoff)))]))

;==============================================================================================
;==== Operation Generation
;==============================================================================================

;UNOP
(define (gen-code-unop out cenv sinfo op rs cenvs)
  (gen-code-recurse out cenv sinfo rs cenvs)
  (match op
    ['minus (display "neg eax\n" out)]
    ['not   (display "not eax\n" out)
            (display "and eax,1\n" out)]))

;BINOP
(define (gen-code-binop out cenv sinfo op ls rs cenvs)
  (comment out "gen-code-binop: " (symbol->string op))
  (push out "ebx" "saving")		;save ebx (cause we gonna use it)
  (cond
    [(or (equal? op 'barbar) (equal? op 'ampamp)) (gen-code-logical out cenv sinfo op ls rs cenvs)]
    [(equal? op 'instanceof) (gen-code-instanceof out cenv sinfo ls rs cenvs)]
    [else
     (if [stringlit? ls] (gen-code-stringlit out cenv sinfo ls cenvs) (gen-code-recurse out cenv sinfo ls cenvs))
     (mov out "ebx" "eax" "save binop rs result")			;move result from above into eax
     (if [stringlit? rs] (gen-code-stringlit out cenv sinfo rs cenvs) (gen-code-recurse out cenv sinfo rs cenvs))
     (match op
       ['plus  (cond
                 [(or (string-rtype? (ast-env ls)) (string-rtype? (ast-env rs)))
                  (str-add out cenvs ls rs)]
                 [else (add out "ebx" "eax")
                       (mov out "eax" "ebx")])]
       ['minus (sub out "ebx" "eax")
               (mov out "eax" "ebx")]
       ['star  (imul out "ebx" "eax")
               (mov out "eax" "ebx")]
       ['slash (divide out "ebx" "eax")
               (mov out "eax" "ebx")]
       ['pct   (rem out "ebx" "eax")
               (mov out "eax" "ebx")]
       [(or 'eqeq 'noteq 'gt 'lt 'gteq 'lteq) (conditional out op "ebx" "eax")
                                              (mov out "eax" "ebx")])])
  
  (pop out "ebx" "restoring"))			;restore ebx

(define (str-add out cenvs ls rs)
  ;ls is in ebx
  ;rs is in eax
  (display "\n\n;STRING ADDITION\n" out)
  (push out "eax")	;push rs on stack
  (gen-methodcall out cenvs '("java" "lang" "String") (funt "valueOf" (args->params (list ls))) "0" "ebx")
  (mov out "ebx" "eax")	;mov valueOf result into ebx
  (pop out "ecx")	;pop rs off stack and put in ecx
  (gen-methodcall out cenvs '("java" "lang" "String") (funt "valueOf" (args->params (list rs))) "0" "ecx")

  (display ";RUNNING CONCAT\n" out)
  ;now know for sure that a string object is in eax (rs) and ebx (ls)
  (gen-methodcall out cenvs '("java" "lang" "String") (funt "concat" (list (rtype '("java" "lang" "String")))) "ebx" "eax")
  (display ";DONE\n\n" out))

;generated METHOD CALL
(define (gen-methodcall out cenvs cls mfunt thisreg argreg)
  (define mcvar (find-codemeth mfunt (codeenv-methods (find-codeenv cls cenvs))))
  (push out argreg)	;push arg
  (push out thisreg) 	;push this
  (call out (mangle-names mcvar))
  (reset-stack out 2))

(define (args->params args)
  (map (lambda(arg) (ast-env arg)) args))

(define (string-rtype? t)
  (and (rtype? t) (equal? '("java" "lang" "String") (rtype-type t))))
  

;Helper - CONDITIONAL
(define (conditional out op reg1 reg2)
  (let ([ltrue (symbol->string (gensym "true"))]	
        [lend (symbol->string (gensym "end"))])
    (cmp out reg1 reg2)				;first compare the 2 operations
    (match op					;then put the correct comditional jmp to a tru label 
      ['eqeq (cjmp out "je" ltrue)]
      ['noteq (cjmp out "jne" ltrue)]
      ['gt (cjmp out "jg" ltrue)]
      ['lt (cjmp out "jl" ltrue)]
      ['gteq (cjmp out "jge" ltrue)]
      ['lteq (cjmp out "jle" ltrue)])
    (movi out reg1 0)				;if the condition was false set eax to 0
    (jmp out lend)
    (label out ltrue)				;and skip passed true code
    (movi out reg1 1)				;if condition was true set eax to 1
    (label out lend)))

;Helper - LOGICAL
(define (gen-code-logical out cenv sinfo op ls rs cenvs)
  (let ([lend (symbol->string (gensym "end"))])
    (gen-code-recurse out cenv sinfo ls cenvs)
    (cmp out "eax" "1")
    (match op
      ['barbar (cjmp out "je" lend)
               (gen-code-recurse out cenv sinfo rs cenvs)
               (label out lend)]
      ['ampamp (cjmp out "jne" lend)
               (gen-code-recurse out cenv sinfo rs cenvs)
               (label out lend)])))

;;helper - INSTANCEOF
;;this code is mostly copied from the gen-code-cast function
(define (gen-code-instanceof out cenv sinfo ls rs cenvs)
  (comment out "Getting lhs of instanceof")
  (gen-code-recurse out cenv sinfo ls cenvs)
  (comment out "We now have lhs of instanceof")
  (match rs
  [(rtype name)
  (let
      ([fail-label (symbol->string (gensym "instanceoffail"))]
       [success-label (symbol->string (gensym "instanceofsuccess"))]
       [end-label (symbol->string (gensym "instanceofend"))]
       [cenv (find-codeenv name cenvs)] )
    
       (cmp out "eax" "0")
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
       (movi out "eax" 1)
       (label out end-label "Done instanceof"))]
    [(atype (rtype name))
	(let ([cenv (find-codeenv name cenvs)]
		      [fail-label (symbol->string (gensym "castfail"))]
		      [success-label (symbol->string (gensym "castablesuccess"))])
		(cond
			[(codeenv-class? cenv) 
			(push out "eax")
			(push out "ebx")		
			(gen-get-array-class-id out "eax")
			(let ([id-list (codeenv-casts cenv)])
			  (gen-check-if-castable out id-list "eax" "ebx" success-label))
			(label out fail-label)
			(movi out "eax" 0)
			(label out success-label "Valid Cast")
			(movi out "eax" 1)				
			(pop out "ebx")
			(pop out "eax")]
			[else (error 'cast-atype-interface "unimplemented")]))]))


(define (stringlit? t)
  (and (literal? t) (equal? (literal-type t) (rtype '("java" "lang" "String")))))



(define (gen-code-stringlit out cenv sinfo slit cenvs)
  (match (assoc (literal-value slit) (stackinfo-strdecls sinfo))
    [(list key value) (movf out "eax" "ebp" (second value))]
    [_ (gen-code-classcreate out cenv sinfo (funt "" (list (atype (ptype 'char)))) '("java" "lang" "String") (list slit) cenvs)]))

;==============================================================================================
;==== Array Generation
;==============================================================================================

(define (arraycreate-sz out cenv sinfo ty size)	;used by us when we know the size of the array
  (push out "ebx")
  (movi out "eax" size)	;used by malloc, 2 is added to this register in gen-arraycreate-code
  (movi out "ebx" size)	;ebx is placed into the array as the array size in gen-arraycreate-code
  (gen-arraycreate-code out)
  (pop out "ebx")) 


;ARRAY CREATE
(define (gen-code-arraycreate out cenv sinfo ty sz cenvs)
  (define no-exception (symbol->string (gensym "noexception")))
  
  (push out "ebx")
  
  (comment out "ARRAY CREATE")
  (gen-code-recurse out cenv sinfo sz cenvs)
  (mov out "ebx" "eax")		;save the size of the array in ebx
  
  (comment out "Checking array size not negative")
  (cmp out "ebx" "0")
  (cjmp out "jge" no-exception)
  (call out "__exception" "array size less than 0")
  (label out no-exception)
  
  (gen-arraycreate-code out)
  (mov out "esi" ARRAY-LABEL)
  (mov out "[eax]" "esi")
  (cond
    [(rtype? ty) (movf out "esi" (mangle-names (find-codeenv (rtype-type ty) cenvs)) "")
                 (mov out "[eax+4]" "esi")]
    [(ptype? ty) (mov out "[eax+4]" "0")]
    [(atype? ty) (error 'gen-code-arraycreate "how?")])
  
  (pop out "ebx"))

(define (gen-arraycreate-code out)
  (add out "eax" ARRAY-HEADER-SZ)	;add 2 to size so we can store the size
  (imul out "eax" "4")
  (push out "ebx")
  (call out "__malloc")		;address will be in eax
  (pop out "ebx")
  (mov out (string-append "[eax+4*"ARRAY-SZ-LOC"]") "ebx"))	;move the size of the array to the first element

;ARRAY ACCESS
(define (gen-code-arrayaccess out cenv sinfo rtnaddr left index cenvs) 
  (define lbl-sz-ok1 (symbol->string (gensym "noexception1")))
  (define lbl-sz-ok2 (symbol->string (gensym "noexception2")))
  (push out "ebx")
  
  (comment out "ARRAY ACCESS")
  (gen-code-recurse out cenv sinfo index cenvs)
  (mov out "ebx" "eax")			;ebx now holds the index offset
  (gen-code-recurse out cenv sinfo left cenvs)	;eax now holds the array address
  
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

(define (gen-code-fieldaccess out cenv sinfo rtnaddr left field cenvs)
  ;(define fcvar (find-codevar field (codeenv-vars (find-codeenv (rtype-type (get-left-type left)) cenvs))))
  (define left-type (get-left-type left))

  (comment out "Fieldaccess")
  (push out "ebx")

  (cond
    [(atype? left-type) (gen-atype-fieldaccess out cenv sinfo left field cenvs)]
    [else (let ([fcvar (find-codevar field (codeenv-vars (find-codeenv (rtype-type (get-left-type left)) cenvs)))])
            (cond
              [(codevar-static? fcvar) (gen-static-fieldaccess out cenv sinfo rtnaddr left fcvar cenvs)]
              [else (gen-normal-fieldaccess out cenv sinfo rtnaddr left fcvar cenvs)]))])
  
  (pop out "ebx"))


(define (gen-atype-fieldaccess out cenv sinfo left field cenvs)
  (cond
    [(equal? field "length") (gen-code-get-this out cenv sinfo left cenvs)		;eax now has addr of begin of array
                             (mov out "eax" (string-append "[eax+4*"ARRAY-SZ-LOC"]"))]	;return the length of the array
    [else (error (string-append "Fieldacces on array type thats not length: " field))]))

(define (gen-static-fieldaccess out cenv sinfo rtnaddr left fcvar cenvs)
  (cond
    [rtnaddr (mov out "eax" (mangle-names fcvar))]				;return the address of the static label
    [else    (mov out "eax" (string-append "[" (mangle-names fcvar) "]"))]))	

(define (gen-normal-fieldaccess out cenv sinfo rtnaddr left fcvar cenvs)
  (gen-code-get-this out cenv sinfo left cenvs)
  (cond
    [rtnaddr (addi out "eax" (codevar-tag fcvar))]				;add the offset to this address
    [else    (mov out "eax" (string-append "[ebx+" (number->string (codevar-tag fcvar)) "]"))]))

(define (get-left-type left)
  (cond
    [(rtype? left) left]
    [else (ast-env left)]))

;==============================================================================================
;==== "this" Generation
;==============================================================================================

(define (gen-code-this out)
  (mov out "eax" "[ebp+8]")) 

(define (gen-code-get-this out cenv sinfo t cenvs)
  (define non-null (symbol->string (gensym "non_null")))
  (comment out "Getting \"this\"")
  
  (cond
    [(rtype? t) (mov out "ebx" "0")		;static call to method, no this
                (jmp out non-null)]		;since static call, no need to check null
    [else (gen-code-recurse out cenv sinfo t cenvs)	;doing a method call on something which better be a class
          (mov out "ebx" "eax")])		
  
  (cmp out "ebx" "0")
  (cjmp out "jne" non-null)
  (call out "__exception")
  (label out non-null))				;local method call, use current this

;==============================================================================================
;==== If / While / For Generation
;==============================================================================================

(define (gen-code-iff out cenv sinfo test tru fls cenvs)
  (comment out "IFF")
  (let  ([label-fls (symbol->string (gensym "if_false"))]
         [label-end-of-if (symbol->string (gensym "if_end"))])
    (comment out "Evaluating test")
    (gen-code-recurse out cenv sinfo test cenvs)		;eval test, eax will contain 0 or 1 (false or true)
    (comment out "Done evaluating test")
    
    (movi out "ecx" 1)				;mov 1 into ecx
    (cmp out "eax" "ecx")			;cmp test to ecx (true)
    (cjmp out "jne" label-fls)			;if test is eqaul to false then jump to the false code
    
    (comment out "TRUE CODE")
    (gen-code-recurse out cenv sinfo tru cenvs)		;else the test was false so run false code
    (jmp out label-end-of-if)			;done fls code so jump passed tru code
    (comment out "End of true code")		
    
    (label out label-fls)
    (comment out "FALSE CODE")
    (gen-code-recurse out cenv sinfo fls cenvs)		;tru code
    (comment out "End of false code") 
    (label out label-end-of-if)))		;end of if statement

(define (gen-code-while out cenv sinfo test body cenvs)
  (let  ([label-cond (symbol->string (gensym "while_check_condition"))]
         [label-end (symbol->string (gensym "while_end"))])
    (label out label-cond)
    (gen-code-recurse out cenv sinfo test cenvs)
    (cmp out "eax" "1")			
    (cjmp out "jne" label-end)			
    (gen-code-recurse out cenv sinfo body cenvs)
    (jmp out label-cond)
    (label out label-end)))

(define (gen-code-for out cenv sinfo init clause update body cenvs)
  (let  ([label-cond (symbol->string (gensym "for_check_condition"))]
         [label-update (symbol->string (gensym "for_update"))]
         [label-end (symbol->string (gensym "for_end"))])
    (comment out "FOR")
    (define new-sinfo (if [varassign? init] (gen-code-recurse out cenv sinfo init cenvs) sinfo))
    (jmp out label-cond)
    (label out label-update)
    (gen-code-recurse out cenv new-sinfo update cenvs)
    (label out label-cond)
    (gen-code-recurse out cenv new-sinfo clause cenvs)
    (movi out "ecx" 1)				
    (cmp out "eax" "ecx")			
    (cjmp out "jne" label-end)			
    (gen-code-recurse out cenv new-sinfo body cenvs)
    (jmp out label-update)
    (label out label-end)
    (reset-stack out (- (length (stackinfo-ldecls new-sinfo)) (length (stackinfo-ldecls sinfo))))))

;==============================================================================================
;==== OTHER
;==============================================================================================

(define (gen-code-varuse-read out cenv sinfo id cenvs)
  (cond
    [(use-membrdecl? cenv sinfo id) (mov out "eax" "[ebp+8]")	;if its a member variable then we need to get if from the this
                                    (mov out "eax" (string-append "[eax+" (get-var-mem-loc cenv sinfo id) "]"))]
    [else (mov out "eax" (string-append "[" (get-var-mem-loc cenv sinfo id) "]"))]))

(define (gen-code-varuse-write out cenv sinfo id)
  (cond
    [(use-membrdecl? cenv sinfo id) (mov out "ecx" "[ebp+8]")	;if its a member variable then we need to get if from the this
                                    (mov out (string-append "[ecx+" (get-var-mem-loc cenv sinfo id) "]") "eax")]
    [else (mov out (string-append "[" (get-var-mem-loc cenv sinfo id) "]") "eax")]))

(define (gen-code-literal out cenv sinfo type val cenvs)
  (match type
    [(ptype 'int) (movi out "eax" val "lit int val " (number->string val))]
    [(ptype 'char) (movi out "eax" val "lit char val " (number->string val))]
    [(ptype 'byte) (movi out "eax" val "lit byte val " (number->string val))]
    [(ptype 'short) (movi out "eax" val "lit short val " (number->string val))]    
    [(ptype 'null) (movi out "eax" 0 "literal val null")]
    [(ptype 'boolean) (movi out "eax" (if val 1 0) "literal val bool")]
    [(rtype '("java" "lang" "String")) (gen-code-classcreate out cenv sinfo (funt "" (list (atype (ptype 'char)))) '("java" "lang" "String") (list (literal empty type val)) cenvs)]))

;;gen-code-cast: output stack-info type ast (listof codeenv) -> void
(define (gen-code-cast out cenv sinfo c ex cenvs)
  ;(printf "gen-code-cast ~a~n" ex)
  (gen-code-recurse out cenv sinfo ex cenvs)
  (match c
    [(ptype 'int)  (comment out "cast to int")]
    [(ptype 'byte) (display "\tmovsx eax, al\t; cast to a byte\n" out)]
    [(ptype 'short) (display "\tmovsx eax, ax\t; cast to a short\n" out)]
    [(ptype 'char) (display "\tmovzx eax, ax\t; cast to a char\n" out)]
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
    [(atype (rtype name))
		(let ([cenv (find-codeenv name cenvs)]
		      [fail-label (symbol->string (gensym "castfail"))]
		      [success-label (symbol->string (gensym "castablesuccess"))])
		(cond
			[(codeenv-class? cenv) 
			(push out "eax")
			(push out "ebx")		
			(gen-get-array-class-id out "eax")
			(let ([id-list (codeenv-casts cenv)])
			  (gen-check-if-castable out id-list "eax" "ebx" success-label))
			(label out fail-label)
			(call out "__exception" "Bad Cast")
			(label out success-label "Valid Cast")
			(pop out "ebx")
			(pop out "eax")]
			[else (error 'cast-atype-interface "unimplemented")]))]
    ))
;==============================================================================================
;==== Helpers
;==============================================================================================

(define (get-var-mem-loc cenv sinfo id)
  (define mdecl (assoc id (stackinfo-mdecls sinfo)))
  (define ldecl (assoc id (stackinfo-ldecls sinfo)))
  (define membr-decl (find-codevar id (codeenv-vars cenv)))
  (cond
    [(and (list? mdecl) (list? ldecl)) (error "We have clashing decls in m and l")]
    [(list? mdecl) (string-append "ebp" (second mdecl))] ;method param, get off stack
    [(list? ldecl) (string-append "ebp" (second ldecl))] ;local variable, get off stack
    [(and (codevar? membr-decl) (codevar-static? membr-decl)) (mangle-names membr-decl)] ;static variable, use label
    [(codevar? membr-decl) (number->string (codevar-tag membr-decl))] ;member variable, use offset into this
    [else (error "Could find a declaration for a variable? No test should be like that.")]))

;determines if the use of id is a member variable (ie no local/static)
(define (use-membrdecl? cenv sinfo id)
  (define mdecl (assoc id (stackinfo-mdecls sinfo)))
  (define ldecl (assoc id (stackinfo-ldecls sinfo)))
  (define membr-decl (find-codevar id (codeenv-vars cenv)))
  (and (not (or (list? mdecl) (list? ldecl))) 
       (and (codevar? membr-decl) (not (codevar-static? membr-decl)))))

(define (stackinfo-add-ldecl sinfo id)
  (define ldecls (stackinfo-ldecls sinfo))
  (define ebpoff (stackinfo-ebpoff sinfo))
  (stackinfo (stackinfo-mdecls sinfo)
             (cons (list id (string-append "-" (number->string ebpoff))) ldecls)
             (stackinfo-strdecls sinfo)
             (+ ebpoff WORD)))

(define (stackinfo-add-strdecl sinfo id ex)
  (cond
    [(and (literal? ex) (equal? (literal-type ex) (rtype '("java" "lang" "String"))))
     (stackinfo (stackinfo-mdecls sinfo)
                (stackinfo-ldecls sinfo)
                (cons (list (literal-value ex) (list id (second (first (stackinfo-ldecls sinfo))))) 
                      (stackinfo-strdecls sinfo))
                (stackinfo-ebpoff sinfo))]
    [else sinfo]))

(define (stackinfo-rmv-modified-strdecl sinfo id)
  (stackinfo (stackinfo-mdecls sinfo)
                (stackinfo-ldecls sinfo)
                (filter-not (lambda(strdecl) (equal? id (first (second strdecl)))) (stackinfo-strdecls sinfo))
                (stackinfo-ebpoff sinfo)))


;;the register points to the object. The caller preserves the register. 
(define (gen-get-class-id out register)
  (movf out register register "" "Getting static class info")
  (movf out register register "" "Getting the class number"))


(define (gen-get-array-class-info out register)
	(display (string-append "\t" "lea " register " [" register "+" (number->string WORD) "]" ";Getting static class info from array") out))


;;The register points to the array. The caller preserves the register.
(define (gen-get-array-class-id out register)
	(movf out  register register "+4" "Getting static array info")
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



(define (gen-initialize-static-fields out cenvs)
  ;(printf "gen-initialize-static-fields: ~a~n" cenvs)
  (define (gen-initialize-static-fields-class cenv)
    (map (lambda (cvar)
           (gen-code-recurse out cenv empty (codevar-val cvar) cenvs) 
           (mov out (string-append "[" (mangle-names cvar)  "]") "eax" "Loading static value for " (mangle-names cvar)))
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

(define (idiv out reg . comment)
  (display (string-append "\tidiv " reg) out)
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

;reg1/reg2
(define (divide out dstreg reg2)
  (comment out "divide")
  (define set-edx-neg (symbol->string (gensym "set_edx_neg")))
  (define end (symbol->string (gensym "end")))

  (mov out "ecx" reg2)	;mov reg2 into ecx
  (mov out "eax" dstreg);mov dstreg into eax
			;now eax/ecx

  ;set edx to 0 or -1 based on sign of eax
  (cmp out "eax" "0")
  (cjmp out "jl" set-edx-neg)
  (mov out "edx" "0")
  (jmp out end)
  (label out set-edx-neg)
  (mov out "edx" "-1")
  (label out end)

  (idiv out "ecx")
  (mov out dstreg "eax"))

(define (rem out dstreg reg2)
  (divide out dstreg reg2)
  (mov out dstreg "edx"))


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
  (display "extern __malloc\n" out))
