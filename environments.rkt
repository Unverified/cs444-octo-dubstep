
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

(provide (struct-out eval))
(provide (struct-out envs))
(provide (struct-out roote))

(struct funt (id argt)   #:prefab)
(struct eval (scope ast) #:prefab)
(struct envs (types vars methods constructors))
(struct roote (id env) #:prefab)

;======================================================================================
;==== Environment Generation
;======================================================================================

(define (mdecl->funt mdecl)
  (match mdecl
    [(methoddecl _ id params) (funt id (map parameter-type params))]
    ;[(methodcall id params) (funt id (map (lambda (x) x) params))]
    [_ (error "mdecl->funt: mdecl that is not a method declaration passed int")]))

(define (gen-root-env asts)
  (map (lambda (x) (list (c-unit-name x) (roote (gensym) (gen-class-envs x)))) asts))

(define (gen-class-envs ast)
  (define (_gen-class-env scope asts envt)
    (match asts
      [`() envt]     
      [`(,(constructor _ scop mdecl _) ,rst ...)     (_gen-class-env scope rst (add-env-const envt mdecl scope (first asts)))]
      [`(,(method _ scop mod type mdecl _) ,rst ...) (_gen-class-env scope rst (add-env-method envt mdecl scope type (first asts)))]
      [`(,(or (var _ _ _ type (varassign _ id _))
              (var _ _ _ type id)) ,rst ...)         (_gen-class-env scope rst (add-env-variable envt id scope type (first asts)))]
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
  (define block-hash (make-hash))
  (define (_va block-id lenv ast)
    (match ast
      [(varuse _ v) lenv]
      [(var _ _ _ type (varassign _ id bdy)) (_va block-id lenv bdy)
                                             (add-env-variable lenv id block-id type ast)]
      
      [(varassign _ id bdy) (_va block-id lenv id)
                            (_va block-id lenv bdy)]
      
      [(while _ test body) (_va block-id lenv test)
                           (_va block-id lenv body)]
      
      [(return _ expr) (_va block-id lenv expr)]
      [(binop _ _ left right) (_va block-id lenv left)
                              (_va block-id lenv right)]
      
      [(unop _ _ right) (_va block-id lenv right)]
      
      [(cast _ c expr)  (_va block-id lenv expr)]
      
      [(or (ptype _ _)
           (rtype _ _)
           (atype _ _)
           (literal _ _ _)
           (methodcall _ _ _)
           (arraycreate _ _ _)
           (fieldaccess _ _ _)
           (arrayaccess _ _ _)
           (classcreate _ _ _)) lenv]
      
      [(iff _ test tru fls) (begin0 (_va block-id lenv test)
                                    (if (empty? tru) (void) (_va block-id lenv tru))
                                    (if (empty? fls) (void) (_va block-id lenv fls))
                                    lenv)]
      
      [(for _ init clause update (block _ id bdy)) (let ([for-envt (_va id lenv init)])
                                                     (hash-set! block-hash id (env-append for-envt cenv))
                                                     (_va id for-envt clause)
                                                     (_va-list id for-envt bdy))]
      
      [(block _ id bdy) (hash-set! block-hash id (env-append lenv cenv))
                        (_va-list id lenv bdy)
                        lenv]
      [(keyword _ _) lenv]
      
      [_ (error ast block-id)]))
  
  (define (_va-list block-id lenv asts)
    (cond
      [(empty? asts) lenv]
      [else  (_va-list block-id (_va block-id lenv (first asts)) (rest asts))]))
  
  (define (_top_va id ast)
    (match ast
      [(or (method _ _ _ _ decl (block _ id bdy))
           (constructor _ _ decl (block _ id bdy))) (let ([lenv (mdecl->envs id decl)])
                                                      (hash-set! block-hash id (env-append lenv cenv))
                                                  (_va-list id lenv bdy))]
      [(var _ _ _ t (varassign _ _ ex))  (_va id env-empty ex)]
      [_ env-empty]))
  (match ast
    [(or (cunit _ _ bdy)
         (class _ _ _ _ _ _ bdy)) (va cenv bdy)]
    [(interface _ _ _ _ _ _) block-hash]
    [(block _ id bdy) (hash-set! block-hash id cenv)
                      (map (curry _top_va id) bdy)
                      block-hash]))

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
'#s(cunit () (#s(pimport ("java" "lang")))
    #s((class ast 0 (1 ())) () public () "test1" () ()
       #s((block ast 0 (1 ())) () g248547
          (#s((constructor ast 0 (1 ())) () public #s((methoddecl ast 0 (1 ())) () "test1" ()) #s((block ast 0 (1 ())) () g248548 ()))
           #s((var ast 0 (1 ())) () public () #s((ptype ast 0 (1 ())) () int) "x")
           #s((var ast 0 (1 ())) () public () #s((ptype ast 0 (1 ())) () int) #s((varassign ast 0 (1 ())) () "y" #s((literal ast 0 (1 ())) () #s((ptype ast 0 (1 ())) () int) 2)))
           #s((method ast 0 (1 ())) () public () #s((ptype ast 0 (1 ())) () int)
              #s((methoddecl ast 0 (1 ())) () "test1" ())
              #s((block ast 0 (1 ())) () g248549
                 (#s((var ast 0 (1 ())) () () () #s((ptype ast 0 (1 ())) () int)
                                        #s((varassign ast 0 (1 ())) () "x" #s((literal ast 0 (1 ())) () #s((ptype ast 0 (1 ())) () int) 12)))
                  #s((block ast 0 (1 ())) () g248551
                     (#s((var ast 0 (1 ())) () () () #s((ptype ast 0 (1 ())) () char)
                         #s((varassign ast 0 (1 ())) () "y" #s((literal ast 0 (1 ())) () #s((ptype ast 0 (1 ())) () char) "'a'")))
                      #s((block ast 0 (1 ())) () g248552
                         (#s((for ast 0 (1 ())) ()
                             #s((var ast 0 (1 ())) () () () #s((ptype ast 0 (1 ())) () int)
                                #s((varassign ast 0 (1 ())) () "i" #s((literal ast 0 (1 ())) () #s((ptype ast 0 (1 ())) () int) 0)))
                             #s((binop ast 0 (1 ())) () lt ("i") ("x"))
                             #s((varassign ast 0 (1 ())) () ("i") #s((binop ast 0 (1 ())) () plus ("i") #s((literal ast 0 (1 ())) () #s((ptype ast 0 (1 ())) () int) 1)))
                             #s((block ast 0 (1 ())) () g248550 (#s((varassign ast 0 (1 ())) () ("x") ("i")))))
                          #s((varassign ast 0 (1 ())) () ("y") #s((literal ast 0 (1 ())) () #s((ptype ast 0 (1 ())) () char) "'b'"))
                          #s((return ast 0 (1 ())) () ("x")))))))))
           #s((method ast 0 (1 ())) () public ()
              #s((ptype ast 0 (1 ())) () int)
              #s((methoddecl ast 0 (1 ())) () "cocks"
                 (#s((parameter ast 0 (1 ())) () #s((ptype ast 0 (1 ())) () int) "number") #s((parameter ast 0 (1 ())) () #s((ptype ast 0 (1 ())) () char) "type")))
              #s((block ast 0 (1 ())) () g248553
                 (#s((return ast 0 (1 ())) ()
                     #s((binop ast 0 (1 ())) () plus ("number") #s((literal ast 0 (1 ())) () #s((rtype ast 0 (1 ())) () ("java" "lang" "String")) "\" cocks\n\"")))))))))))

(define (t1-classenv)(gen-class-envs test1))
(define (t1-locals) (va (t1-classenv) test1))
(define (do-test1) (hash-for-each (t1-locals) (lambda (k v)
                              (printf "~n~a~n==================~n" k)
                              (envs-print v))))



