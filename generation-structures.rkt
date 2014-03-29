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

(define (name->id name)
  (hash-ref! store name (thunk (counter))))

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

(define (assoclst->codemeth local lookup lst)
  (reverse (for/list ([off (range 1 (add1 (length lst)))]
                      [asc (reverse-normalize lst)])
             (codemeth (first asc)
                       (equal? local (eval-scope (second asc)))
                       (is-static? (eval-ast (second asc)))
                       (first (hash-ref lookup (eval-scope (second asc)) (thunk (error (eval-scope (second asc)) " not in lookup"))))
                       (* 4 off) 
                       (eval-ast (second asc))))))

(define (assoclst->codevars local lookup lst)
  (define (get-assignment ast)
    (cond [(varassign? ast) (varassign-expr ast)]
          [(vdecl? ast) empty]
          [else (let-values ([(type _) (struct-info ast)]
                             [(err)  (open-output-string)])
                  (display "gen-strctrs:assoclst->codevars:get-assignent no case " err)
                  (display ast err)
                  (error (get-output-string err)))]))
  
  (let-values ([(static instance) (partition (compose1 is-static? eval-ast second) lst)])    
    (append (map (lambda (x) (codevar (first x)
                                      (equal? local (eval-scope (second x)))
                                      #t                         
                                      (first (hash-ref lookup (eval-scope (second x)) (thunk (error (eval-scope (second x)) " not in lookup"))))
                                      (get-assignment (eval-ast (second x))))) static)
            (reverse (for/list ([off (range 1 (add1 (length instance)))]
                                [asc  (reverse instance)])
                       (codevar (first asc)
                                #t
                                #f 
                                (* 4 off)
                                (get-assignment (eval-ast (second asc)))))))))

(define (get-parent cinfo)
  (if (equal? (info-name cinfo) '("java" "lang" "Object"))
      empty
      (let ([parent (get-extends (info-ast cinfo))])
        (if (empty? parent)
            '("java" "lang" "Object")
            parent))))

(define (info->codeenv all-info cinfo)
  (let* ([lookup (make-immutable-hash (map (lambda (x) (list (cunit-scope (info-ast x)) (info-name x))) all-info))]
         [vars (assoclst->codevars (cunit-scope (info-ast cinfo)) lookup (envs-vars (info-env cinfo)))])
    (codeenv
     (info-name cinfo)
     (name->id (info-name cinfo))
     (cond [(is-class? (info-ast cinfo)) #t]
           [(is-interface? (info-ast cinfo)) #f]
           [else (error "info->codeenv givencinfo of a improper compilation unit")])
     (* 4 (+ 1 (length (filter-not codevar-static? vars))))
     (get-parent cinfo)
     vars
     (assoclst->codemeth (cunit-scope (info-ast cinfo)) lookup (append (envs-constructors (info-env cinfo))
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
