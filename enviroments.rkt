#lang racket

(require "ast-tree.rkt")

(provide print-envs)
(provide gen-root-env)
(provide gen-class-envs)
(provide (struct-out envs))

(struct funt (id argt) #:transparent)
(struct envs (vars types methods constructors) #:transparent)

(define (c-unit-name ast)
  (match ast
    [(or (cunit package _ (class _ _ id _ _ _)) 
         (cunit package _ (interface _ _ id _ _))) (append package (list id))]
    [_ (error "c-unit->env requires a ")]))

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
     
      [`(,(constructor scop mdecl _) ,rst ...)     (let* ([const-call (mdecl->funt mdecl)]
                                                          [const-envt (envs empty empty empty `((,const-call ,scope)))])
                                                     (if (false? (assoc const-call (envs-constructors envt)))
                                                      (_gen-class-env scope rst (env-append envt const-envt))
                                                      (error "duplicate constructor defined")))]
      
      
      [`(,(method scop mod type mdecl _) ,rst ...) (let* ([method-call (mdecl->funt mdecl)]
                                                          [method-envt (envs empty `((,method-call ,scope)) `((,method-call ,type)) empty)])
                                                     (if (false? (assoc method-call (envs-methods envt)))
                                                      (_gen-class-env scope rst (env-append envt method-envt))
                                                      (error "duplicate method defined")))]
      [`(,(or (var _ _ type (varassign id _))
              (var _ _ type id)) ,rst ...)         (let ([var-envt (envs `((,id ,scope)) empty `((,id ,type)) empty)])  
                                                     (if  (false? (assoc id (envs-vars envt)))
                                                          (_gen-class-env scope rst (env-append envt var-envt))
                                                          (error "duplicate field variable name defined")))]
      [`(,_ ,rst ...) (_gen-class-env scope rst envt)]))
  
    (match ast
      [(cunit _ _ b) (gen-class-envs b)]
      [(class _ _ id _ _ b) (let ([e (gen-class-envs b)])
                              (envs (envs-vars e) (envs-methods e) (cons (list id 'class) (envs-types e)) (envs-constructors e)))]
      [(interface _ _ id _ b) (let ([e (gen-class-envs b)])
                              (envs (envs-vars e) (envs-methods e) (cons (list id 'interface) (envs-types e)) (envs-constructors e)))]
      [(block id bdy) (_gen-class-env id bdy env-empty)]))

;======================================================================================
;==== Environment Transformation
;======================================================================================
;(: env-append-1 : envs envs -> envs )
(define (env-append-1 le re)
  (envs (append (envs-vars le) (envs-vars re))
        (append (envs-methods le) (envs-methods re))
        (append (envs-types le) (envs-types re))
        (append (envs-constructors le) (envs-constructors re))))

;(: env-append : envs envs... -> envs )
(define (env-append le . r)
  (foldr env-append-1 le r))

;======================================================================================
;==== Bases
;======================================================================================
(define env-empty (envs empty empty empty empty))

;==============================================================================================
;==== Print Functions
;==============================================================================================

(define (print-env env)
  (printf "~a~n" (first env)))

(define (print-envs envs)
  (for-each (lambda (env) (print-env env)) envs))




(define test1 (cunit '() '() (class 'public '() "test1" '() '(("java" "io" "Serializable"))
  (block
   'g47330
   (list (constructor 'public (methoddecl "test1" '()) (block 'g47331 '()))
         (var 'public '() (ptype 'int) "x")
         (var 'public '() (ptype 'int) (varassign "y" "2"))
         (method 'public '(static) (ptype 'int) (methoddecl "test" '()) (block 'g47332 (list (return "123"))))
         )))))

(define (gen-test1)
  (gen-class-envs test1))

(gen-test1)