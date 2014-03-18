#lang racket

(provide (struct-out ptype))
(provide (struct-out rtype))
(provide (struct-out atype))
(provide (struct-out ftype))

(provide type-whole?)
(provide type-numeric?)
(provide type-ast=?)
(provide type-string?)
(provide type-bool?)

(provide type-string)

;;(define-type type (ptype rtype atype ftype))
;(struct ptype ([type : Symbol]))
(struct ptype (type) #:prefab)
;(struct rtype ([type : Symbol]))
(struct rtype (type) #:prefab)
;(struct atype ([type : type]))
(struct atype (type) #:prefab)
;(struct ftype ([type : type]))
(struct ftype (type) #:prefab)

;;(: type-whole? : type -> Bool )
(define (type-whole? type)
  (let ([number-ptype? (compose1 list? (curryr member '(int char byte short)))])
    (cond
      [(ptype? type) (number-ptype? (ptype-type type))]
      [else #f])))

;(: type-numeric? : type -> Boolean )
(define (type-numeric? type)
  (let ([numeric-ptype? (compose1 list (curryr member '(int short char byte long float double)))])
    (cond
      [(ptype? type) (numeric-ptype? (ptype-type type))]
      [else #f])))

;(: type-ast=? : type type -> Boolean )
(define (type-ast=? t1 t2)
  (printf "type-ast=? ~a ~a~n" t1 t2)
  (match (list t1 t2)
    [`(,(ptype ta) ,(ptype tb)) (symbol=? ta tb)]
    [`(,(rtype ta) ,(rtype tb)) (equal? ta tb)]
    [`(,(atype ta) ,(atype tb)) (type-ast=? ta tb)]
    [_ #f]))

(define type-string (rtype '("java" "lang" "String")))
(define type-string? (curry type-ast=? type-string))

(define type-bool (ptype 'boolean))
(define type-bool? (curry type-ast=? type-bool))
