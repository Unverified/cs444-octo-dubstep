#lang racket

(require "types.rkt")
(require "class-info.rkt")
(require "ast-tree.rkt")
(require "environments.rkt")

(provide (struct-out codevar))
(provide (struct-out codemeth))
(provide (struct-out codeenv))

(provide info->codeenv)


;======================================================================================
;==== Code Environment Generation
;======================================================================================

;; tag depends on how static is set, 
;; true means its a label id, false is an offset
(struct codevar (id static? tag val) #:transparent)
(struct codemeth (id scope off def) #:transparent)
;; type is either 'inter or 'class
(struct codeenv (name class? vars methods) #:transparent
  #:guard (lambda (name cls vars meths type-name)
            (cond [(not (list? name)) (error type-name "invalid class/interface name ~e" name)]
                  [(not (boolean? cls)) (error type-name "invalid class indicator ~e" class?)]
                  [else (values name class vars meths)])))

(define (reverse-normalize asoc)
  (cond [(empty? asoc) empty]
        [else (for/list ([key (remove-duplicates (map first (reverse asoc)))])
                (assoc key asoc))]))
  
(define (assoclst->codemeth lst)
  (reverse (for/list ([off (range 1 (add1 (length lst)))]
                      [asc (reverse-normalize lst)])
             (codemeth (first asc) (eval-scope (second asc)) (* 4 off) (eval-ast (second asc))))))

(define (assoclst->codevars lst)
  (define (get-assignment ast)
    (cond [(varassign? ast) (varassign-expr ast)]
          [(vdecl? ast) empty]
          [else (let-values ([(type _) (struct-info ast)]
                             [(err)  (open-output-string)])
                  (display "gen-strctrs:assoclst->codevars:get-assignent no case " err)
                  (display ast err)
                  (error (get-output-string err)))]))
  (let-values ([(static instance) (partition (compose1 is-static? eval-ast second) lst)])
    (append 
     (map (lambda (x) (codevar (first x) #t (eval-scope (second x)) (get-assignment (eval-ast (second x))))) static)
     (reverse (for/list ([off (range 1 (add1 (length instance)))]
                         [asc  (reverse instance)])
                (codevar (first asc) #f (* 4 off) (get-assignment (eval-ast (second asc)))))))))

(define (info->codeenv cinfo)
  (codeenv
   (info-name cinfo)
   (cond [(is-class? (info-ast cinfo)) #t]
         [(is-interface? (info-ast cinfo)) #f]
         [else (error "info->codeenv givencinfo of a improper compilation unit")])
   (assoclst->codevars (envs-vars (info-env cinfo)))
   (append (assoclst->codemeth (append (envs-constructors (info-env cinfo))
                                       (envs-methods (info-env cinfo)))))))


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
