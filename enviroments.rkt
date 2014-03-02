
#lang racket

(require "ast-tree.rkt")

(provide envs-print)
(provide gen-root-env)
(provide gen-class-envs)
(provide (struct-out envs))

(struct funt (id argt)   #:transparent )
(struct eval (scope ast) #:transparent )

(struct envs (types vars methods constructors) #:transparent)

;======================================================================================
;==== Environment Generation
;======================================================================================

(define (mdecl->funt mdecl)
  (match mdecl
    [(methoddecl id params) (funt id (map parameter-type params))]
    ;[(methodcall id params) (funt id (map (lambda (x) x) params))]
    [_ (error "mdecl->funt: mdecl that is not a method declaration passed int")]))

(define (gen-root-env asts)
  (map (lambda (x) (list (c-unit-name x) (gen-class-envs x))) asts))

(define (gen-class-envs ast)
  (define (_gen-class-env scope asts envt)
    (match asts
      [`() envt]     
      [`(,(constructor scop mdecl _) ,rst ...)     (_gen-class-env scope rst (add-env-const envt mdecl scope (first asts)))]
      [`(,(method scop mod type mdecl _) ,rst ...) (_gen-class-env scope rst (add-env-method envt mdecl scope type (first asts)))]
      [`(,(or (var _ _ type (varassign id _))
              (var _ _ type id)) ,rst ...)         (_gen-class-env scope rst (add-env-variable envt id scope type (first asts)))]
      [`(,_ ,rst ...) (_gen-class-env scope rst envt)]))
  (match ast
    [(cunit _ _ b) (gen-class-envs b)]
    [(or (class _ _ id _ _ b)
         (interface _ _ id _ b)) (let ([e (gen-class-envs b)])
                                   (envs (cons (list id ast) (envs-types e)) (envs-vars e) (envs-methods e) (envs-constructors e)))]
    [(block id bdy) (_gen-class-env id bdy env-empty)]))

(define (mdecl->envs scope decl)
  (define (params->envs envt params)
    (cond [(empty? params) envt]
          [else (match-let ([(parameter type id) (first params)])
                  (params->envs (add-env-variable envt id scope type (first params)) (rest params)))]))
  (params->envs env-empty (methoddecl-parameters decl)))

(define (va tenv cenv ast)
  (define (_va block-id lenv ast)
    (match ast
      [`(,var) (if (false? (assoc var (envs-vars lenv)))
                   (error (string-append "declaraition made to " var " which doesn't exist!"))
                   lenv)]            
      [(var _ _ type (varassign id bdy)) (begin0 (_va block-id lenv bdy)
                                                 (_va block-id (add-env-variable lenv id block-id type ast)))]
      [(varassign id bdy)                (begin0 (_va block-id lenv id)
                                                 (_va block-id lenv bdy))]
      [(while test body)                 (begin0 (_va block-id lenv test)
                                                 (_va block-id lenv body))]
      
      [(return expr)        (_va block-id lenv expr)]
      [(binop _ left right) (begin0 (_va block-id lenv left)
                                    (_va block-id lenv right))]
      [(unop _ right)       (_va block-id lenv right)]
      [(cast c expr)        (_va block-id lenv expr)]
      
      [(or (ptype _)
           (rtype _)
           (atype _)
           (literal _ _)
           (methodcall _ _)
           (arraycreate _ _)
           (fieldaccess _ _)
           (arrayaccess _ _)
           (classcreate _ _)) lenv]
      
      [(iff test tru fls) (begin0 (_va block-id lenv test)
                                  (_va block-id lenv tru)
                                  (_va block-id lenv fls))]
      [(block id bdy)  (_va-list id lenv bdy)]))
  
  (define (_va-list block-id lenv asts)
    (cond
      [(empty? asts) lenv]
      [else  (_va-list block-id (_va block-id lenv (first asts)) (rest asts))]))
  
  (define (_top_va id ast)
    (match ast
      [(or (method _ _ _ decl (block id bdy))
           (constructor _ decl (block id bdy))) (_va-list id (mdecl->envs id decl) bdy)]
      [(var _ _ t (varassign _ ex))  (_va id env-empty ex)]
      [_ env-empty]))
  
  (match ast
    [(or (cunit _ _ bdy)
         (class _ _ _ _ _ bdy)) (va tenv cenv bdy)]
    [(interface _ _ _ _ _)                        empty]
    [(block id bdy)             (map (curry _top_va id) bdy)]))

;======================================================================================
;==== Environment Transformation
;======================================================================================
;(: env-append-1 : envs envs -> envs )
(define (env-append-1 le re)
  (envs (append (envs-types le) (envs-types re))
        (append (envs-vars le) (envs-vars re))
        (append (envs-methods le) (envs-methods re))
        (append (envs-constructors le) (envs-constructors re))))

;(: env-append : envs envs... -> envs )
(define (env-append le . r)
  (foldr env-append-1 le r))

(define (add-env-const envt mdecl scope ast)
  (let ([key (mdecl->funt mdecl)]
        [value (eval scope ast)])
    (if (false? (assoc key (envs-constructors envt)))
        (env-append envt (envs empty empty empty `((,key ,value))))
        (error "adding constructor to enviroment that contains same type"))))

(define (add-env-method envt mdecl scope return-type ast)
  (let ([key (mdecl->funt mdecl)]
        [value (eval scope ast)])
    (if (false? (assoc key (envs-methods envt)))
        (env-append envt (envs `((,key ,return-type)) empty `((,key ,value)) empty))
        (error "adding method to enviroment that contains same method"))))

(define (add-env-variable envt var-name scope type ast)
  (let ([value (eval scope ast)])
    (if (false? (assoc var-name (envs-vars envt)))
        (env-append envt (envs `((,var-name ,type)) `((,var-name ,value)) empty empty))
        (error "adding variable to enviroment that contains same name"))))

;======================================================================================
;==== Bases
;======================================================================================
(define env-empty (envs empty empty empty empty))

;==============================================================================================
;==== Print Functions
;==============================================================================================

(define (ast-type-print ast)
  (define (print-type type mod)
    (cond [(empty? mod) (printf "~a" type)]
          [else (printf "~a ~a" mod type)]))
  
  (match ast
    [(class _ mod _ _ _ _)   (print-type "class" mod)]
    [(interface _ mod _ _ _) (print-type "interface" mod)]
    [_ (printf "~a" ast)]))

(define (funt-print f)
  (match f
    [(funt id argt) (printf "~a~a" id argt)]
    [_ (printf "~a" f)]))

(define (envs-print e)
  (printf "Types~n")
  (for-each (lambda (x) 
              (printf "(")
              (funt-print (first x))
              (printf ", ")
              (ast-type-print (second x))
              (printf ")~n")) (envs-types e))
  (printf "~nVars~n")
  (for-each (lambda (x) (printf "(~a, ~a)~n" (first x) (eval-scope (second x)))) 
            (envs-vars e))
  (printf "~nConstructors~n")
  (for-each (lambda (x)
              (printf "(")
              (funt-print (first x))
              (printf ", ~a)~n" (eval-scope (second x)))) (envs-constructors e))
  (printf "~nMethods~n")
  (for-each (lambda (x)
              (printf "(")
              (funt-print (first x))
              (printf ", ~a)~n" (eval-scope (second x)))) (envs-methods e)))

;==============================================================================================
;==== Testing Function
;==============================================================================================
(define test1
  (cunit '() '()
         (class 'public '() "test1" '() '(("java" "io" "Serializable"))
           (block 'g103103
                  (list
                   (constructor 'public (methoddecl "test1" '()) (block 'g103104 '()))
                   (var 'public '() (ptype 'int) "x")
                   (var 'public '() (ptype 'int) (varassign "y" (literal (ptype 'int) "2")))
                   (method 'public '() (ptype 'int) (methoddecl "test1" '()) (block 'g103105 (list (return (literal (ptype 'int) "123")))))
                   (method 'public '() (ptype 'int) (methoddecl "cocks" (list (parameter (ptype 'int) "number") (parameter (ptype 'char) "type")))
                           (block 'g103106 
                                  (list 
                                   (return (binop 'plus '("number") (literal (rtype '("java" "lang" "String")) "\" cocks\n\""))))))
                   )))))

(define (gen-test1) (gen-class-envs test1))
(envs-print (gen-test1))

(define do-test1 (va env-empty (gen-test1) test1))
