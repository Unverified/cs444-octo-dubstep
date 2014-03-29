#lang racket

(require "types.rkt")
(require "class-info.rkt")
(require "ast-tree.rkt")
(require "environments.rkt")

(provide (struct-out codevar))
(provide (struct-out codemeth))
(provide (struct-out codeenv))

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
        [else (for/list ([key (remove-duplicates (map first (reverse asoc)))])
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
                       (get-toplevel (first asc) (info-ast (find-info origin cinfos))))))))

(define (assoclst->codevars local lookup cinfos lst)
  (define (get-assignment ast)
    (cond [(varassign? ast) (varassign-expr ast)]
          [(vdecl? ast) empty]
          [else (let-values ([(type _) (struct-info ast)]
                             [(err)  (open-output-string)])
                  (display "gen-strctrs:assoclst->codevars:get-assignentno case " err)
                  (display ast err)
                  (error (get-output-string err)))]))
  
  (define (asc->codevar static? off asc)
    (let ([origin (first (hash-ref lookup (eval-scope (second asc)) (thunk (error (eval-scope (second asc)) " not in lookup"))))])
      (codevar (first asc)
               (if static? (equal? local (eval-scope (second asc))) #t)
               static?
               (if static? origin off)
               (get-assignment (get-toplevel (first asc) (info-ast (find-info origin cinfos)))))))
  
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

(define (info->codeenv all-info cinfo)
  (let* ([lookup (make-immutable-hash (map (lambda (x) (list (cunit-scope (info-ast x)) (info-name x))) all-info))]
         [vars (assoclst->codevars (cunit-scope (info-ast cinfo)) lookup all-info (envs-vars (info-env cinfo)))])
    (codeenv
     (info-name cinfo)
     (name->id (info-name cinfo))
     (cond [(is-class? (info-ast cinfo)) #t]
           [(is-interface? (info-ast cinfo)) #f]
           [else (error "info->codeenv givencinfo of a improper compilation unit")])
     (* 4 (+ 1 (length (filter-not codevar-static? vars))))
     (get-parent cinfo)
     vars
     (assoclst->codemeth (cunit-scope (info-ast cinfo)) lookup all-info (append (envs-constructors (info-env cinfo))
                                                                                (envs-methods (info-env cinfo))))
     (append (for/list ([name  (map info-name all-info)]
                        [supers (map info-supers all-info)]
                        #:when (list? (member (info-name cinfo) supers)))
               (name->id name))
             (for/list ([name  (map info-name all-info)]
                        [impls (map info-impls all-info)]
                        #:when (and (list? impls) (list? (member (info-name cinfo) impls))))
               (name->id name))))))


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

(define (get-toplevel id ast)
  (define (matches-id? ast)
    (cond [(string? id) (match ast
                          [(varassign _ lft _) (matches-id? lft)]
                          [(vdecl _ _ _ _ i) (equal? i id)]
                          [_ #f])]
          [(funt? id) (match ast
                        [(constructor _ _ decl _) (equal? id (funt "" (funt-argt (mdecl->funt decl))))]
                        [(method _ _ _ _ decl _) (equal? id (mdecl->funt decl))]
                        [_ #f])]))
  (match ast
    [(cunit _ _ bdy) (get-toplevel id bdy)]
    [(or (class _ _ _ _ _ _ bdy)
         (interface _ _ _ _ _ bdy)) (get-toplevel id bdy)]
    [(block _ id bdy) (match (filter matches-id? bdy)    
                        [(list x) x]
                        [empty (error 'get-toplevel "asked for thing that doesn't exist")]
                        [_ (error 'get-toplevel "multiple definitons of type: ~e" id)])]))
