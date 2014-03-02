
#lang racket

(require "ast-tree.rkt")

(provide envs-print)
(provide gen-root-env)
(provide gen-class-envs)
(provide va)

(provide (struct-out envs))

(struct funt (id argt)   #:transparent )
(struct eval (scope ast) #:transparent )

(struct envs (types vars methods constructors) #:prefab)

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
  
  (with-handlers ([exn:fail? (lambda (exn) 
                               (printf "~a" (exn-message exn))
                               (exit 42))])
    (match ast
      [(cunit _ _ b) (gen-class-envs b)]
      [(or (class _ _ id _ _ b)
           (interface _ _ id _ b)) (let ([e (gen-class-envs b)])
                                     (envs (cons (list id ast) (envs-types e)) (envs-vars e) (envs-methods e) (envs-constructors e)))]
      [(block id bdy) (_gen-class-env id bdy env-empty)])))

(define (mdecl->envs scope decl)
  (define (params->envs envt params)
    (cond [(empty? params) envt]
          [else (match-let ([(parameter type id) (first params)])
                  (params->envs (add-env-variable envt id scope type (first params)) (rest params)))]))
  (params->envs env-empty (methoddecl-parameters decl)))

(define (va tenv cenv ast)
  (define block-hash (make-hash))
  (define (_va block-id lenv ast)
    (match ast
      [`(,var) (if (false? (assoc var (envs-vars lenv)))
                   (error (string-append "declaraition made to " var " which doesn't exist!"))
                   lenv)]
      
      [(var _ _ type (varassign id bdy)) (_va block-id lenv bdy)
                                         (add-env-variable lenv id block-id type ast)]
      
      [(varassign id bdy) (_va block-id lenv id)
                          (_va block-id lenv bdy)]
      
      [(while test body) (_va block-id lenv test)
                         (_va block-id lenv body)]
      
      [(return expr) (_va block-id lenv expr)]
      [(binop _ left right) (_va block-id lenv left)
                            (_va block-id lenv right)]
      [(unop _ right) (_va block-id lenv right)]
      [(cast c expr) (_va block-id lenv expr)]
      
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
      
      [(for init clause update (block id bdy)) (let ([for-envt (_va id lenv init)])
                                                 (hash-set! block-hash id (env-append for-envt cenv))
                                                 (_va id for-envt clause)
                                                 (_va-list id for-envt bdy))]
      
      [(block id bdy) (hash-set! block-hash id (env-append lenv cenv))
                      (_va-list id lenv bdy)
                      lenv]))
  
  (define (_va-list block-id lenv asts)
    (cond
      [(empty? asts) lenv]
      [else  (_va-list block-id (_va block-id lenv (first asts)) (rest asts))]))
  
  (define (_top_va id ast)
    (match ast
      [(or (method _ _ _ decl (block id bdy))
           (constructor _ decl (block id bdy))) (let ([lenv (mdecl->envs id decl)])
                                                  (hash-set! block-hash id (env-append lenv cenv))
                                                  (_va-list id lenv bdy))]
      [(var _ _ t (varassign _ ex))  (_va id env-empty ex)]
      [_ env-empty]))
  
  (with-handlers ([exn:fail? (lambda (exn) 
                               (printf "~a" (exn-message exn))
                               (exit 42))])
    (match ast
      [(or (cunit _ _ bdy)
           (class _ _ _ _ _ bdy)) (va tenv cenv bdy)]
      [(interface _ _ _ _ _) block-hash]
      [(block id bdy) (hash-set! block-hash id cenv)
                      (map (curry _top_va id) bdy)
                      block-hash])))

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
  (foldr env-append-1 env-empty (cons le r)))

;(: add-env-const : envs methoddeclaration symbol ast -> envs )
(define (add-env-const envt mdecl scope ast)
  (let ([key (mdecl->funt mdecl)]
        [value (eval scope ast)])
    (if (false? (assoc key (envs-constructors envt)))
        (env-append (envs empty empty empty `((,key ,value))) envt)
        (error "adding constructor to enviroment that contains same type"))))

;(: add-env-method : envs methoddeclaration symbol type ast -> envs )
(define (add-env-method envt mdecl scope return-type ast)
  (let ([key (mdecl->funt mdecl)]
        [value (eval scope ast)])
    (if (false? (assoc key (envs-methods envt)))
        (env-append (envs `((,key ,return-type)) empty `((,key ,value)) empty) envt)
        (error "adding method to enviroment that contains same method"))))

;(: add-env-variable : envs string symbol type ast -> envs )
(define (add-env-variable envt var-name scope type ast)
  (let ([value (eval scope ast)])
    (if (false? (assoc var-name (envs-vars envt)))
        (env-append (envs `((,var-name ,type)) `((,var-name ,value)) empty empty) envt)
        (error "adding variable to enviroment that contains same name"))))

;======================================================================================
;==== Bases
;======================================================================================
(define env-empty (envs empty empty empty empty))

;==============================================================================================
;==== Print Functions
;==============================================================================================

;(: ast-type-print : ast -> void ) 
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
  (cunit '() '(#s(pimport ("java" "lang")))
         (class 'public '() "test1" '() '()
           (block 'g187796
                  (list
                   '#s(constructor public #s(methoddecl "test1" ()) #s(block g187797 ()))
                   '#s(var public () #s(ptype int) "x")
                   (var 'public '() '#s(ptype int) (varassign "y" (literal '#s(ptype int) 2)))
                   (method 'public '() '#s(ptype int) '#s(methoddecl "test1" ())
                           (block 'g187798
                                  (list
                                   (var '() '() '#s(ptype int) (varassign "x" (literal '#s(ptype int) 12)))
                                   (block 'g187800
                                          (list
                                           (var '() '() '#s(ptype char) (varassign "y" (literal '#s(ptype char) "'a'")))
                                           (block 'g187801
                                                  (list
                                                   (for
                                                       (var '() '() '#s(ptype int) (varassign "i" (literal '#s(ptype int) 0)))
                                                        '#s(binop lt ("i") ("x"))
                                                        (varassign '("i") (binop 'plus '("i") (literal '#s(ptype int) 1)))
                                                     '#s(block g187799 (#s(varassign ("x") ("i")))))
                                                   (varassign '("y") (literal '#s(ptype char) "'b'"))
                                                   '#s(return ("x")))))))))
                   (method 'public '() '#s(ptype int) '#s(methoddecl "cocks" (#s(parameter #s(ptype int) "number") #s(parameter #s(ptype char) "type")))
                    (block 'g187802 
                           (list (return (binop 'plus '("number") (literal '#s(rtype ("java" "lang" "String")) "\" cocks\n\"")))))))))))

(define (gen-test1) (gen-class-envs test1))
(envs-print (gen-test1))

(define (do-test1) (va env-empty (gen-test1) test1))
(hash-for-each (do-test1) (lambda (k v)
                           (printf "~n~a~n==================~n" k)
                          (envs-print v)))
