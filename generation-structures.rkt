#lang racket

(require "types.rkt")
(require "class-info.rkt")
(require "ast-tree.rkt")
(require "environments.rkt")

(provide (struct-out codevar))
(provide (struct-out codemeth))
(provide (struct-out codeenv))

(provide name->id)
(provide info->codeenv)
(provide find-codeenv)
(provide find-codemeth)
(provide find-codevar)

(define counter (let ([count 0]) (lambda () (set! count (add1 count)) count)))

(define store (make-hash))
(define (name->id name) (hash-ref! store name (thunk (counter))))

;======================================================================================
;==== Code Environment Generation
;======================================================================================

;; tag depends on how static is set, 
;; true means its the origin class, false is an offset
(struct codevar (id ref? static? tag val) #:transparent)
(struct codemeth (id ref? static? origin off def) #:transparent)
;; type is either 'inter or 'class
(struct codeenv (name guid class? size parent vars methods casts) #:transparent)

(define (reverse-normalize asoc)
  (cond [(empty? asoc) empty]
        [else (for/list ([key (remove-duplicates (reverse (map first asoc)))])
                (assoc key asoc))]))

(define (assoclst->codemeth local lookup cinfos lst)
  (reverse (for/list ([off (range 1 (add1 (length lst)))]
                      [asc (reverse-normalize lst)])
             (let ([origin (first (hash-ref lookup (eval-scope (second asc)) (thunk (error (eval-scope (second asc)) " not in lookup"))))])
               (codemeth (first asc)
                         (equal? local (eval-scope (second asc)))
                         (is-static? (eval-ast (second asc)))
                         origin
                         (* 4 off)
                         (loosetype-expr cinfos (get-toplevel (first asc) (info-ast (find-info origin cinfos)))))))))

(define (assoclst->codevars local lookup cinfos lst)
  (define (get-assignment at)
    (cond [(varassign? at) (varassign-expr at)]
          [(vdecl? at) empty]
          [else (let-values ([(type _) (struct-info at)]
                             [(err)  (open-output-string)])
                  (display "gen-strctrs:assoclst->codevars:get-assignentno case " err)
                  (display at err)
                  (error (get-output-string err)))]))
  
  (define (asc->codevar static? off asc)
    (let ([origin (first (hash-ref lookup (eval-scope (second asc)) (thunk (error (eval-scope (second asc)) " not in lookup"))))])
      (codevar (first asc)
               (if static? (equal? local (eval-scope (second asc))) #t)
               static?
               (if static? origin off)
               (loosetype-expr cinfos (get-assignment (get-toplevel (first asc) (info-ast (find-info origin cinfos))))))))
  
  (let-values ([(static instance) (partition (compose1 is-static? eval-ast second) lst)])    
    (append (map (curry asc->codevar #t 0) static)
            (reverse (for/list ([off (range 1 (add1 (length instance)))]
                                [asc  (reverse instance)])
                       (asc->codevar #f (* 4 off) asc))))))

(define (get-parent cinfo)
  (if (equal? (info-name cinfo) '("java" "lang" "Object"))
      empty
      (let ([parent (get-extends (info-ast cinfo))])
        (if (empty? parent)
            '("java" "lang" "Object")
            parent))))


(define (build-ml all-info cinfo)
    (cond [(false? cinfo) empty]
          [else (append (filter (compose1 (curry equal? (cunit-scope (info-ast cinfo))) eval-scope second) (envs-methods (info-env cinfo)))
                                (build-ml all-info (find-info (get-parent cinfo) all-info)))]))

(define (info->codeenv all-info cinfo)
  (let* ([lookup (make-immutable-hash (map (lambda (x) (list (cunit-scope (info-ast x)) (info-name x))) all-info))]
         [vars (assoclst->codevars (cunit-scope (info-ast cinfo)) lookup all-info (envs-vars (info-env cinfo)))]
         [meths (assoclst->codemeth (cunit-scope (info-ast cinfo)) lookup all-info (append (envs-constructors (info-env cinfo)) (build-ml all-info cinfo)))]
         [array (if (extends-array? (info-name cinfo)) (list (name->id "array")) empty)])
    (codeenv
     (info-name cinfo)
     (name->id (info-name cinfo))
     (cond [(is-class? (info-ast cinfo)) #t]
           [(is-interface? (info-ast cinfo)) #f]
           [else (error 'info->codeenv "given info of a improper compilation unit ~e" (info-name cinfo))])
     
     (* 4 (if (is-interface? (info-ast cinfo))
              (add1 (length meths))
              (add1 (length (filter-not codevar-static? vars)))))
     (get-parent cinfo)
     vars
     meths
     (append array
             (list (name->id (info-name cinfo)))
             (for/list ([name  (map info-name all-info)]
                        [supers (map info-supers all-info)]
                        #:when (list? (member (info-name cinfo) supers)))
               (name->id name))
             (for/list ([name  (map info-name all-info)]
                        [impls (map info-impls all-info)]
                        #:when (and (list? impls) (list? (member (info-name cinfo) impls))))
               (name->id name))))))

(define (extends-array? name)
  (ormap (curry equal? name) (list `("java" "lang" "Object")
                                   `("java" "lang" "Cloneable")
                                   `("java" "io" "Serializable"))))


;======================================================================================
;==== Code Environment Lookup
;======================================================================================

(define (find-codeenv name lst)
  (findf (compose (curry equal? name) codeenv-name) lst))

(define (find-codemeth id lst)
  (cond [(not (funt? id)) (error 'find-codemeth "id must be a funt")]
        [else (findf (compose (curry equal? id) codemeth-id) lst)]))

(define (find-codevar id lst)
  (cond [(not (string? id)) (error 'find-codevar "id must be of type string")]
        [else (findf (compose (curry equal? id) codevar-id) lst)]))

;======================================================================================
;==== Environment Generation Helpers
;======================================================================================

(define (get-toplevel id at)
  (define (matches-id? at)
    (cond [(string? id) (match at
                          [(varassign _ lft _) (matches-id? lft)]
                          [(vdecl _ _ _ _ i) (equal? i id)]
                          [_ #f])]
          [(funt? id) (match at
                        [(constructor _ _ decl _) (equal? id (funt "" (funt-argt (mdecl->funt decl))))]
                        [(method _ _ _ _ decl _) (equal? id (mdecl->funt decl))]
                        [_ #f])]))
  (match at
    [(cunit _ _ bdy) (get-toplevel id bdy)]
    [(or (class _ _ _ _ _ _ bdy)
         (interface _ _ _ _ _ bdy)) (get-toplevel id bdy)]
    [(block _ id bdy) (match (filter matches-id? bdy)    
                        [(list x) x]
                        [empty (error 'get-toplevel "asked for thing that doesn't exist")]
                        [_ (error 'get-toplevel "multiple definitons of type: ~e" id)])]))

(define (get-field-type all-cinfo left field)
  (define (_get-field-type name)
    (match (assoc field (envs-types (info-env (find-info name all-cinfo))))
      [`(,_ ,t) t]
      [#f (error 'get-field-type "undefined field ~e in ~e" field name)]))
  (cond [(rtype? left) (_get-field-type (rtype-type left))]
        [(ast? left) (get-field-type all-cinfo (ast-env left) field)]
        [(and (atype? left) (equal? field "length")) (ptype 'int)]
        [else (error 'get-field-type "invalid field access type ~e ~e" left field)]))

(define (get-method-type all-cinfo env left id)
  (define (_get-method-type env)
    (match (assoc id (envs-types env))
      [`(,_ ,y) y]
      [_ (error 'get-method-type "unbound method ~e" id)]))
  (cond
    [(empty? left) (_get-method-type env)]
    [(rtype? left) (_get-method-type (info-env (find-info (rtype-type left) all-cinfo)))]
    [(ast? left) (get-method-type all-cinfo env (ast-env left) id)]
    [else (error 'get-method-type "" left)]))

(define (get-binop-type op T1 T2)
  (match (list op T1 T2)
    [`(plus ,(rtype '("java" "lang" "String")) ,_) T1]
    [`(plus ,_ ,(rtype '("java" "lang" "String"))) T2]
    [`(plus ,_ ,_) (ptype 'int)]
    [`(minus ,_ ,_) (ptype 'int)]
    [`(star ,_ ,_) (ptype 'int)]
    [`(slash ,_ ,_) (ptype 'int)]
    [`(pct ,_ ,_) (ptype 'int)]
    [`(eqeq ,_ ,_) (ptype 'boolean)]
    [`(noteq ,_ ,_) (ptype 'boolean)]
    [`(gt ,_ ,_) (ptype 'boolean)]
    [`(gteq ,_ ,_) (ptype 'boolean)]
    [`(lt ,_ ,_) (ptype 'boolean)]
    [`(lteq ,_ ,_) (ptype 'boolean)]
    [`(instanceof ,_ ,_) (ptype 'boolean)]
    [`(barbar ,_ ,_) (ptype 'boolean)]
    [`(ampamp ,_ ,_) (ptype 'boolean)]
    [`(bar ,_ ,_) (ptype 'boolean)]
    [`(amp ,_ ,_) (ptype 'boolean)]))

(define (get-unop-type op T1)
  (match (list op T1)
    [`(minus ,_) (ptype 'int)]
    [`(not ,_) (ptype 'boolean)]))

(define (loosetype-expr all-cinfo at)
  (define voidt (ptype 'void))
  (match at
    [`() `()]
    [(this _ type) (this type type)]
    [(vdecl _ scope mod type id) (vdecl type scope mod type id)]
    [(literal _ type val) (literal type type val)]
    [(varuse env id) (match (assoc id (envs-types env))
                       [`(,x ,y) (varuse y id)]
                       [_ (error 'loosetype-expr "varuse is unbound")])]
    [(varassign _ id expr) (let ([newid (loosetype-expr all-cinfo id)])
                             (varassign (ast-env newid) newid (loosetype-expr all-cinfo expr)))]
    
    [(or (ptype _) (atype _ ) (rtype  _)) at]
    [(parameter _ type id) (parameter type type id)]
    
    [(cast _ c expr) (cast c c (loosetype-expr all-cinfo expr))]
    [(binop _ op left right) (let ([nleft (loosetype-expr all-cinfo left)]
                                   [nright (loosetype-expr all-cinfo right)]
                                   [get-type (lambda (x) (cond [(ptype? x) x]
                                                               [(rtype? x) x]
                                                               [(atype? x) x]
                                                               [(ast? x) (ast-env x)]))])
                               (binop (get-binop-type op (get-type nleft) (get-type nright)) op nleft nright))]
    [(unop _ op right) (let ([nright (loosetype-expr all-cinfo right)])
                         (unop (get-unop-type op (ast-env nright)) op nright))]
    
    [(classcreate _ class params) (let ([nparams (map (curry loosetype-expr all-cinfo) params)])
                                    (classcreate class (funt "" (map ast-env nparams)) nparams))]
    [(arraycreate _ type size) (arraycreate (atype type) type (loosetype-expr all-cinfo size))]
    
    [(arrayaccess _ left index) (let ([newleft (loosetype-expr all-cinfo left)])
                                  (arrayaccess (atype-type (ast-env newleft)) newleft (loosetype-expr all-cinfo index)))]
    
    [(fieldaccess _ left field) (let ([newleft (loosetype-expr all-cinfo left)])
                                  (fieldaccess (get-field-type all-cinfo newleft field) newleft field))]
    
    [(constructor _ scope mdecl bdy) (constructor empty scope mdecl (loosetype-expr all-cinfo bdy))]
    [(method _ scope mod type mdecl bdy) (method type scope mod type mdecl (loosetype-expr all-cinfo bdy))]
    [(methodcall env left id args) (let* ([nargs (map (curry loosetype-expr all-cinfo) args)]
                                          [nleft (loosetype-expr all-cinfo left)]
                                          [mid (funt id (map ast-env nargs))])
                                     (methodcall (get-method-type all-cinfo env nleft mid) nleft mid nargs))]
    
    [(block _ id statements) (block voidt id (map (curry loosetype-expr all-cinfo) statements))]
    [(iff _ test tru fls) (iff voidt (loosetype-expr all-cinfo test) (loosetype-expr all-cinfo tru) (loosetype-expr all-cinfo fls))]
    [(while _ test body) (while voidt (loosetype-expr all-cinfo test) (loosetype-expr all-cinfo body))]
    [(for _ in cl up bdy) (for voidt (loosetype-expr all-cinfo in) (loosetype-expr all-cinfo cl) (loosetype-expr all-cinfo up) (loosetype-expr all-cinfo bdy))]
    
    
    [(return _ '()) (return voidt empty)]
    [(return _ expr) (let ([newexpr (loosetype-expr all-cinfo expr)]) (return (ast-env newexpr) newexpr))]
    
    [_ (error 'loosetype-expr "not implemented for ~e" at)]
    ))
