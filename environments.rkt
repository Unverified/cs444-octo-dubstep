
#lang racket

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
(provide (struct-out roote))

(struct roote (id env) #:prefab)
(struct funt (id argt)   #:prefab)
(struct eval (scope ast) #:prefab)

(struct envs (types vars methods constructors))

;======================================================================================
;==== Environment Generation
;======================================================================================

(define (mdecl->funt mdecl)
  (match mdecl
    [(methoddecl _ id params) (funt id (map parameter-type params))]
    ;[(methodcall id params) (funt id (map (lambda (x) x) params))]
    [_ (error "mdecl->funt: mdecl that is not a method declaration passed int")]))


(define (methodcall->funt mcall type-expr)
  (match mcall
    [(methodcall _ id params) (funt id (map type-expr params))]
    [_ (error "Not a methodcall")]))

(define (gen-root-env asts)
  (map (lambda (x) (list (c-unit-name x) (roote (gensym) (gen-class-envs x)))) asts))

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
      [(varuse _ v) (set-ast-envt! ast (env-append lenv cenv))
                    lenv]
      
      [(vdecl _ _ _ type id) (let ([new-env (add-env-variable lenv id block-id type ast)])
                               (set-ast-envt! ast (env-append new-env cenv))
                               new-env)]
      
      [(varassign _ id bdy) (set-ast-envt! ast (env-append lenv cenv))
                            (_va block-id lenv bdy)
                            (_va block-id lenv id)]
      
      [(while _ test body) (set-ast-envt! ast (env-append lenv cenv))
                           (_va block-id lenv test)
                           (_va block-id lenv body)]
      
      [(return _ expr) (set-ast-envt! ast (env-append lenv cenv))
                       (_va block-id lenv expr)]
      
      [(binop _ _ left right) (set-ast-envt! ast (env-append lenv cenv))
                              (_va block-id lenv left)
                              (_va block-id lenv right)]
      
      [(unop _ _ right) (set-ast-envt! ast (env-append lenv cenv))
                        (_va block-id lenv right)]
      
      [(cast _ c expr)  (set-ast-envt! ast (env-append lenv cenv))
                        (_va block-id lenv expr)]
      
      [(or (ptype _ _)
           (rtype _ _)
           (atype _ _)
           (literal _ _ _)
           (methodcall _ _ _)
           (arraycreate _ _ _)
           (fieldaccess _ _ _)
           (arrayaccess _ _ _)
           (classcreate _ _ _)) (set-ast-envt! ast (env-append lenv cenv)) 
                                lenv]
      
      [(iff _ test tru fls) (set-ast-envt! ast (env-append lenv cenv))
                            (_va block-id lenv test)
                            (if (empty? tru) (void) (_va block-id lenv tru))
                            (if (empty? fls) (void) (_va block-id lenv fls))
                            lenv]
      
      [(for _ init clause update (block _ id bdy)) (let ([for-envt (_va id lenv init)])
                                                     (set-ast-envt! ast (env-append for-envt cenv))
                                                     (_va id for-envt clause)
                                                     (_va-list id for-envt bdy))]
      
      [(block _ id bdy) (set-ast-envt! ast (env-append lenv cenv))
                        (_va-list id lenv bdy)
                        lenv]
      
      [_ (error ast block-id)]))
  
  (define (_va-list block-id lenv asts)
    (cond [(empty? asts) lenv]
          [else  (_va-list block-id (_va block-id lenv (first asts)) (rest asts))]))
  
  (define (_top_va id ast)
    (match ast
      [(or (method _ _ _ _ decl (block _ id bdy))
           (constructor _ _ decl (block _ id bdy))) (let ([lenv (mdecl->envs id decl)])
                                                      (set-ast-envt! ast (env-append lenv cenv))
                                                      (_va-list id lenv bdy))]
      [(vdecl _ _ _ _ _) cenv]
      [(varassign _ _ ex)  (_va id env-empty ex)]
      [_ env-empty]))
  (match ast
    [(cunit _ _ bdy) (va cenv bdy)]
    [(or (class _ _ _ _ _ _ bdy)
         (interface _ _ _ _ _ bdy)) (set-ast-envt! ast cenv)
                                    (va cenv bdy)]
    [(block _ id bdy) (set-ast-envt! ast cenv)
                      (map (curry _top_va id) bdy)
                      (void)]))

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

;==============================================================================================
;==== Testing Function
;==============================================================================================

(define test1
  '#s(cunit
      ()
      (#s(pimport ("java" "lang")))
      #s((class ast 0 (1 ())) () public () "Je_2_Locals_Overlapping_DeeplyNested" () ()
                              #s((block ast 0 (1 ())) () g68509
                                                      (#s((constructor ast 0 (1 ())) () public #s((methoddecl ast 0 (1 ())) () "Je_2_Locals_Overlapping_DeeplyNested" ())
                                                                                     #s((block ast 0 (1 ())) () g68510 ()))
                                                       #s((method ast 0 (1 ())) () public (static)
                                                          #s((ptype ast 0 (1 ())) () int)
                                                          #s((methoddecl ast 0 (1 ())) () "test" ())
                                                          #s((block ast 0 (1 ())) ()  g68511
                                                             (#s((varassign ast 0 (1 ())) ()
                                                                                          #s((vdecl ast 0 (1 ())) () () () #s((ptype ast 0 (1 ())) () int) "a")
                                                                                          #s((literal ast 0 (1 ())) () #s((ptype ast 0 (1 ())) () int) 123))
                                                              #s((block ast 0 (1 ())) () g68514
                                                                 (#s((varassign ast 0 (1 ())) () 
                                                                                              #s((vdecl ast 0 (1 ())) () () () #s((ptype ast 0 (1 ())) () boolean) "b")
                                                                                              #s((literal ast 0 (1 ())) () #s((ptype ast 0 (1 ())) () bool) "true"))
                                                                  #s((block ast 0 (1 ())) ()  g68515
                                                                     (#s((varassign ast 0 (1 ())) ()
                                                                                                  #s((vdecl ast 0 (1 ())) () () () #s((ptype ast 0 (1 ())) () boolean) "c")
                                                                                                  #s((literal ast 0 (1 ())) () #s((ptype ast 0 (1 ())) () bool) "true"))
                                                                      #s((block ast 0 (1 ())) () g68516
                                                                         (#s((varassign ast 0 (1 ())) ()
                                                                                                      #s((vdecl ast 0 (1 ())) () () () #s((ptype ast 0 (1 ())) () boolean) "d")
                                                                                                      #s((literal ast 0 (1 ())) () #s((ptype ast 0 (1 ())) () bool) "true"))
                                                                          #s((block ast 0 (1 ())) () g68517
                                                                                                  (#s((varassign ast 0 (1 ())) ()
                                                                                                                               #s((vdecl ast 0 (1 ())) () () () #s((ptype ast 0 (1 ())) () boolean) "e")
                                                                                                                               #s((literal ast 0 (1 ())) () #s((ptype ast 0 (1 ())) () bool) "true"))
                                                                              #s((block ast 0 (1 ())) () g68518
                                                                                 (#s((varassign ast 0 (1 ())) ()
                                                                                                              #s((vdecl ast 0 (1 ())) () () () #s((ptype ast 0 (1 ())) () boolean) "f")
                                                                                                              #s((literal ast 0 (1 ())) () #s((ptype ast 0 (1 ())) () bool) "true"))
                                                                                  #s((block ast 0 (1 ())) () g68519
                                                                                     (#s((iff ast 0 (1 ())) ()
                                                                                         #s((varuse ast 0 (1 ())) () "b")
                                                                                         #s((iff ast 0 (1 ())) () #s((varuse ast 0 (1 ())) () "c")
                                                                                            #s((iff ast 0 (1 ())) () #s((varuse ast 0 (1 ())) () "d")
                                                                                               #s((iff ast 0 (1 ())) () #s((varuse ast 0 (1 ())) () "e")
                                                                                                  #s((iff ast 0 (1 ())) () #s((varuse ast 0 (1 ())) () "f")
                                                                                                     #s((block ast 0 (1 ())) () g68512
                                                                                                        (#s((varassign ast 0 (1 ())) ()
                                                                                                                                     #s((vdecl ast 0 (1 ())) () () () #s((ptype ast 0 (1 ())) () int) "a")
                                                                                                                                     #s((literal ast 0 (1 ())) () #s((ptype ast 0 (1 ())) () int) 43))
                                                                                                         #s((block ast 0 (1 ())) ()  g68513
                                                                                                            (#s((return ast 0 (1 ())) ()
                                                                                                                #s((binop ast 0 (1 ())) () plus  #s((varuse ast 0 (1 ())) () "a")
                                                                                                                   #s((literal ast 0 (1 ())) () #s((ptype ast 0 (1 ())) () int) 80)))))))
                                                                                                     ())
                                                                                                  ())
                                                                                               ())
                                                                                            ())
                                                                                         ())
                                                                                      #s((return ast 0 (1 ())) () #s((varuse ast 0 (1 ())) () "a"))))))))))))))))))))))

(define (t1-classenv)(gen-class-envs test1))
(define (t1-locals) (va (t1-classenv) test1))
(define (do-test1) (hash-for-each (t1-locals) (lambda (k v)
                                                (printf "~n~a~n==================~n" k)
                                                (envs-print v))))



