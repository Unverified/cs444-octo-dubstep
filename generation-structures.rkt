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
(struct codevar (id static? tag) #:transparent)
(struct codemeth (id scope off) #:transparent)
;; type is either 'inter or 'class
(struct codeenv (name vars methods) #:transparent)

(define (reverse-normalize asoc)
  (cond [(empty? asoc) empty]
        [else (for/list ([key (remove-duplicates (map first (reverse asoc)))])
                (assoc key asoc))]))
  
(define (assoclst->codemeth lst)
  (reverse (for/list ([off (range 1 (add1 (length lst)))]
                      [asc (reverse-normalize lst)])
             (codemeth (first asc) (eval-scope (second asc)) off))))

(define (assoclst->codevars lst)
  (let-values ([(static instance) (partition (compose1 is-static? eval-ast second) lst)])
    (append 
     (map (lambda (x) (codevar (first x) #t (eval-scope (second x)))) static)
     (reverse (for/list ([off (range 1 (add1 (length instance)))]
                         [asc  (reverse instance)])
                (codevar (first asc) #f (* 4 off)))))))
             
(define (info->codeenv cinfo)
  (codeenv
   (info-name cinfo)
   (assoclst->codevars (envs-vars (info-env cinfo)))
   (append (assoclst->codemeth (append (envs-constructors (info-env cinfo))
                                       (envs-methods (info-env cinfo)))))))