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
      [`(,(constructor scop mdecl _) ,rst ...)     (let* ([const-call (mdecl->funt mdecl)]
                                                          [value (eval scope (first asts))]
                                                          [const-envt (envs empty empty empty `((,const-call ,value)))])
                                                     (if (false? (assoc const-call (envs-constructors envt)))
                                                         (_gen-class-env scope rst (env-append envt const-envt))
                                                         (error "duplicate constructor defined")))]
      
      [`(,(method scop mod type mdecl _) ,rst ...) (let* ([method-call (mdecl->funt mdecl)]                                                   
                                                          [value (eval scope (first asts))]
                                                          [method-envt (envs `((,method-call ,type)) empty `((,method-call ,value)) empty)])
                                                     (if (false? (assoc method-call (envs-methods envt)))
                                                         (_gen-class-env scope rst (env-append envt method-envt))
                                                         (error "duplicate method defined")))]
      [`(,(or (var _ _ type (varassign id _))
              (var _ _ type id)) ,rst ...)         (let* ([value (eval scope (first asts))]
                                                          [var-envt (envs `((,id ,type)) `((,id ,value)) empty empty)])  
                                                     (if  (false? (assoc id (envs-vars envt)))
                                                          (_gen-class-env scope rst (env-append envt var-envt))
                                                          (error "duplicate field variable name defined")))]
      [`(,_ ,rst ...) (_gen-class-env scope rst envt)]))
  (match ast
    [(cunit _ _ b) (gen-class-envs b)]
    [(or (class _ _ id _ _ b)
         (interface _ _ id _ b)) (let ([e (gen-class-envs b)])
                                   (envs (cons (list id ast) (envs-types e)) (envs-vars e) (envs-methods e) (envs-constructors e)))]
   [(block id bdy) (_gen-class-env id bdy env-empty)]))

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
  (for-each (lambda (x) (printf "(~a, ~a)~n" (first x) (eval-scope (second x)))) (envs-vars e))
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
