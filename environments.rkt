
#lang racket

(require "errorf.rkt")
(require "ast-tree.rkt")

(provide env-empty)
(provide envs-print)
(provide gen-root-env)
(provide gen-class-envs)

(provide env-append-nocons)
(provide env-append)

(provide va)
(provide env-empty)

(provide methodcall->funt)
(provide (struct-out eval))
(provide (struct-out envs))

(struct funt (id argt)   #:prefab)
(struct eval (scope ast) #:prefab)
(struct envs (types vars methods constructors))

;======================================================================================
;==== Environment Generation
;======================================================================================

(define (mdecl->funt mdecl)
  (match-let ([(methoddecl _ id params) mdecl])
    (funt id (map parameter-type params))))

(define (methodcall->funt mcall F)
  (match mcall
    [(methodcall _ _ id params) (funt id (map F params))]
    [_ (error "Not a methodcall")]))

(define (gen-root-env asts)
  (map (lambda (x) (list (c-unit-name x) (gen-class-envs x))) asts))

(define (gen-class-envs ast)
  (define (_gen-class-env scope asts envt)
    (match asts
      [`() envt] 
      [`(,(constructor _ scop mdecl _) ,rst ...)     (_gen-class-env scope rst (add-env-const envt mdecl scope (first asts)))]
      [`(,(method _ scop mod type mdecl _) ,rst ...) (_gen-class-env scope rst (add-env-method envt mdecl scope type (first asts)))]
      [`(,(or (varassign _ (vdecl _ _ _ type id) _)
              (vdecl _ _ _ type id)) ,rst ...)       (_gen-class-env scope rst (add-env-variable envt id scope type (first asts)))]
      [`(,_ ,rst ...) (_gen-class-env scope rst envt)]))
  (match ast
    [(cunit _ _ b) (gen-class-envs b)]
    [(or (class _ _ _ id _ _ b)
         (interface _ _ _ id _ b)) (let ([e (gen-class-envs b)])
                                     (envs (cons (list id ast) (envs-types e)) (envs-vars e) (envs-methods e) (envs-constructors e)))]
    [(block _ id bdy) (_gen-class-env id bdy env-empty)]))

(define (mdecl->envs scope decl)
  (define (params->envs envt params)
    (cond [(empty? params) envt]
          [else (match-let ([(parameter _ type id) (first params)])
                  (params->envs (add-env-variable envt id scope type (first params)) (rest params)))]))
  (params->envs env-empty (methoddecl-parameters decl)))

(define (va cenv ast)
  (define (_va block-id lenv ast)
    (match ast
      [(varuse _ v) (varuse (env-append cenv lenv) v)]
      
      [(vdecl _ x y type id) (let ([new-env (add-env-variable lenv id block-id type ast)])
                               (vdecl new-env x y type id))]
      
      [(varassign _ id bdy) (let ([newid (_va block-id lenv id)])
                              (varassign (ast-env newid) newid (_va block-id (ast-env newid) bdy)))]
      
      [(while _ test body) (while (env-append cenv lenv)
                                  (_va block-id lenv test)
                                  (_va block-id lenv body))]

      [(return _ expr) (return (env-append cenv lenv)
                               (_va block-id lenv expr))]
      
      [(binop _ op left right) (binop (env-append cenv lenv)
                                     op
                                     (_va block-id lenv left)
                                     (_va block-id lenv right))]
      
      [(unop _ op right) (unop (env-append cenv lenv)
                              op
                              (_va block-id lenv right))]
      
      [(cast _ c expr) (cast (env-append cenv lenv)
                              c
                              (_va block-id lenv expr))]
      
      [(ambiguous _ ids) (ambiguous (env-append cenv lenv) ids)]
     
      [(this _ t) (this (env-append cenv lenv) t)]
      [(literal _ t val) (literal (env-append cenv lenv) t val)]
      
      [(methodcall _ left id args) (methodcall (env-append lenv cenv) (_va block-id lenv left) id (_va-list block-id lenv args))]
      [(arraycreate _ t size) (arraycreate (env-append lenv cenv) t (_va block-id lenv size))]
      [(fieldaccess _ left id) (fieldaccess (env-append lenv cenv) (_va block-id lenv left) id)]
      [(arrayaccess _ left access) (arrayaccess (env-append lenv cenv) (_va block-id lenv left) (_va block-id lenv access))]
      [(classcreate _ class params) (classcreate (env-append lenv cenv) (_va block-id lenv class) (_va-list block-id lenv params))]
      
      [(iff _ test tru fls) (iff (env-append lenv cenv)
                                 (_va block-id lenv test)
                                 (if (empty? tru) empty (_va block-id lenv tru))
                                 (if (empty? fls) empty (_va block-id lenv fls)))]
      
      [(for _ init clause update (block _ id bdy)) (let ([for-envt (_va id lenv init)])
                                                     (for (env-append (ast-env for-envt) cenv)
                                                          for-envt
                                                          (_va id (ast-env for-envt) clause)
                                                          (_va id (ast-env for-envt) update)
                                                          (block (ast-env for-envt) id (_va-list id (ast-env for-envt) bdy))))]
      
      [(block _ id bdy) (block (env-append lenv cenv)
                               id
                               (_va-list id lenv bdy))]
      
      [_ ast]))
  
  (define (_va-list block-id lenv asts)
    (cond [(empty? asts) empty]
          [else  (define _va-statement (_va block-id lenv (first asts)))
                 (cons _va-statement   (_va-list block-id (if (block? _va-statement) lenv (ast-env _va-statement)) (rest asts)))]))
  
  (define (_top_va id ast)
    (match ast
      [(method _ s m t decl bdy) (let ([lenv (mdecl->envs id decl)])
                                                      (method (env-append lenv cenv) s m t decl
                                                              (_va block-id lenv bdy)))]
      [(constructor _ id decl bdy) (let ([lenv (mdecl->envs id decl)])
                                                      (constructor (env-append lenv cenv) id decl
                                                                   (_va block-id lenv bdy)))]
      [(vdecl _ s m t md) (vdecl cenv s m t md)]
      [(varassign _ id ex) (varassign cenv id (_va id env-empty ex))]))
  
  (match ast
    [(cunit package imports bdy) (cunit package imports (va cenv bdy))]
    [(class _ s m id exts impls bdy) (class cenv s m id exts impls (va cenv bdy))]
    [(interface _ s m id exts bdy) (interface cenv s m id exts (va cenv bdy))]
    [(block _ id bdy) (block cenv id (map (curry _top_va id) bdy))]))

;======================================================================================
;==== Environment Transformation
;======================================================================================

;(: env-append : envs envs... -> envs )
(define (env-append le . r)
  ;(: env-append-1 : envs envs -> envs )
  (define (env-append-1 le re)
    (envs (append (envs-types le) (envs-types re))
          (append (envs-vars le) (envs-vars re))
          (append (envs-methods le) (envs-methods re))
          (append (envs-constructors le) (envs-constructors re))))
  (foldr env-append-1 env-empty (cons le r)))

;(: env-append-nocons : envs envs... -> envs )
;; like env-append but will only carry though the constructors of the leftmost environment
(define (env-append-nocons le . r)
  (apply env-append (cons le (map (lambda (x) (envs (envs-types x) (envs-vars x) (envs-methods x) empty)) r))))

;(: add-env-const : envs methoddeclaration symbol ast -> envs )
(define (add-env-const envt mdecl scope ast)
  (let ([key (mdecl->funt mdecl)]
        [value (eval scope ast)])
    (if (false? (assoc key (envs-constructors envt)))
        (env-append (envs empty empty empty `((,key ,value))) envt)
        (c-errorf "adding constructor to enviroment that contains same type"))))

;(: add-env-method : envs methoddeclaration symbol type ast -> envs )
(define (add-env-method envt mdecl scope return-type ast)
  (let ([key (mdecl->funt mdecl)]
        [value (eval scope ast)])
    (if (false? (assoc key (envs-methods envt)))
        (env-append (envs `((,key ,return-type)) empty `((,key ,value)) empty) envt)
        (c-errorf "adding method to enviroment that contains same method"))))

;(: add-env-variable : envs string symbol type ast -> envs )
(define (add-env-variable envt var-name scope type ast)
  (envs-print envt)
  (printf "VAR: ~a~n" var-name)
  (let ([value (eval scope ast)])
    (if (false? (assoc var-name (envs-vars envt)))
        (env-append (envs `((,var-name ,type)) `((,var-name ,value)) empty empty) envt)
        (c-errorf "adding variable to enviroment that contains same name"))))

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
    [(class _ _ mod _ _ _ _)   (print-type "class" mod)]
    [(interface _ _ mod _ _ _) (print-type "interface" mod)]
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
