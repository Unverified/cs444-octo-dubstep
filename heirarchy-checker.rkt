#lang racket

(require "errorf.rkt")
(require "class-info.rkt")
(require "environments.rkt")
(require "ast-tree.rkt")

(provide type-ast=?)
(provide check-heirarchies)


(define (type-ast=? t1 t2)
  (match (list t1 t2)
    [`(,(ptype _ ta) ,(ptype _ tb)) (equal? ta tb)]
    [`(,(rtype _ ta) ,(rtype _ tb)) (equal? ta tb)]
    [`(,(atype _ ta) ,(atype _ tb)) (type-ast=? ta tb)]
    [_ #f]))

;======================================================================================
;==== Getter Helpers
;======================================================================================

(define (get-ast-extends ast)
  (define extends (get-extends ast))
  (define java-lang-Object (list "java" "lang" "Object")) 
  (cond
    [(and (empty? extends) (not (equal? (c-unit-name ast) java-lang-Object))) java-lang-Object]
    [else extends]))

(define (get-info fullname root)
  (second (assoc fullname root)))

;======================================================================================
;==== Heirarchy Checking
;======================================================================================

(define (check-heriarchy root cinfo)
  (cond
    [(is-class? (info-ast cinfo)) (gen-full-class-info root cinfo empty)]
    [(is-interface? (info-ast cinfo)) (gen-full-interface-info root cinfo empty)]))

(define (check-interface-info seen-so-far cinfo)
  (let ([name (c-unit-name (info-ast cinfo))])
    (cond
      [(not (is-interface? (info-ast cinfo))) (c-errorf "Must implement an interface.")]
      [else cinfo])))

;check that the infos interface and that it does not exist in seen-so-far
(define (check-interfaces-info seen-so-far infos)
  (cond [(empty? infos) empty]
        [else (let ([name (c-unit-name (info-ast (first infos)))])
                (cond [(occurs? name seen-so-far) (c-errorf "~a has duplicate definition" name)]
                      [else (cons (check-interface-info seen-so-far (first infos)) 
                                  (check-interfaces-info (cons name seen-so-far) (rest infos)))]))]))

;check that l links to a class and that it does not exist in parent-extds
(define (check-class-info subclasses cinfo)
  (cond [(empty? cinfo) empty]
        [(empty? (info-ast cinfo)) (info empty env-empty empty)]
        [else (let ([name (c-unit-name (info-ast cinfo))])
                (cond [(not (is-class? (info-ast cinfo))) (c-errorf "Must extend a class.")]
                      [(is-class-with-mod? (info-ast cinfo) 'final) (c-errorf "Cannot extend a final class.")]
                      [else cinfo]))]))

(define (lookup-cunit-env F subs root fullname)
  (cond [(empty? fullname) (info empty env-empty empty)]
        [else (match (assoc fullname root)
                [`(,name ,info) (F root info subs)]
                [_ (error "full name not found, how did i get here?")])]))

(define (gen-full-class-info root cinfo subclasses)
  (let ([c-ast (info-ast cinfo)])
    (printf "Checking Class Heir of ~a~n" (c-unit-name c-ast))
    (define classpath (extend-path (c-unit-name c-ast) subclasses))
    (define extends (get-ast-extends c-ast))
    (printf "----extends ~a~n" (string-join extends "."))
    
    (define implements (get-implements c-ast))
    (printf "----implements ~a~n" (map (curryr string-join ".") implements))
    
    (define superclass-info (check-class-info classpath (lookup-cunit-env gen-full-class-info classpath root extends)))
    (define interface-infos (check-interfaces-info empty (map (curry lookup-cunit-env gen-full-interface-info empty root) implements)))
    (define interface-envs (map info-env interface-infos))
    (printf "----got intEnvs ~a~n" interface-envs)
    
    (define cenv (combine-envs (info-env superclass-info) (info-env cinfo)))
    (printf "----got cenv ~a~n" cenv)
    
    (define fullenv (foldr combine-envs cenv interface-envs))
    (printf "----got fullEnv ~a~n" fullenv)
    
    (info c-ast (check-for-abstract c-ast fullenv) (info-links cinfo))))

;check a class for proper heirarchy
;  (define (get-class-heriarchy ast links extds)
;    (printf "CHECKING CLASS HEIR FOR: ~a~n" (c-unit-name ast))
;    (define parent-extds (cons (c-unit-name ast) extds))
;    (define extends (get-ast-extends ast))
;    (define implements (get-implements ast))
;    (printf "--- EXTENDS: ~a~n" extends)
;    (printf "--- IMPLEMENTS: ~a~n" implements)
;    (define class-link (check-class-link (assoc extends links) parent-extds))
;    (define interface-links (check-interface-links (map identity implements) empty))
; "DO STUFF HERE"    
;    (define extends-env (get-extends-env class-link parent-extds))
;    (define interface-envs (map (lambda(x) (get-interface-env x empty)) interface-links))
;    (define cur-class-env (combine-envs extends-env (get-rootenv (c-unit-name ast) links)))
;    (define return-env (foldr combine-envs cur-class-env interface-envs))
;    (check-for-abstract ast return-env))

(define (extend-path newele sub-path)
  (printf "extending-path ~a to ~a~n" (string-join newele ".") (map (curryr string-join ".") sub-path))
  (cond [(occurs? newele sub-path) (c-errorf "Cycle detected ~a" (map (curryr string-join ".") (cons newele sub-path)))]
        [else (cons newele sub-path)]))

(define (gen-full-interface-info root cinfo subimpls)
  (let ([c-ast (info-ast cinfo)])
    (printf "Checking Interface Heir For ~a~n" (c-unit-name c-ast))
    (define implpath (extend-path (c-unit-name c-ast) subimpls))
    (define extends (get-extends c-ast))
    (printf "- - EXtends ~a~n" (map (curryr string-join ".") extends))
    (define interface-infos (check-interfaces-info implpath (map (curry lookup-cunit-env gen-full-interface-info implpath root) extends)))
    (define interface-envs (map info-env interface-infos))
    (printf "- - got intEnvs ~a~n" interface-envs)
    
    (define fullenv (foldr combine-envs (info-env cinfo) (check-empty-interface-envs root (info-env cinfo) interface-envs)))
    
    (info c-ast fullenv (info-links cinfo))))

(define (check-empty-interface-envs root cenv interface-envs)
  (cond
    [(envs? (combine-envs (info-env (second (assoc (list "java" "lang" "Object") root))) cenv)) interface-envs]
    [else interface-envs]))
      
;check an interface for proper heirarchy
;(define (get-interface-heriarchy ast links impls)
; (printf "CHECKING INTERFACE HEIR FOR: ~a~n" (c-unit-name ast))
; (define parent-impls (cons (c-unit-name ast) impls))
; (define extends (get-extends ast))
; (printf "--- EXTENDS: ~a~n" extends)
; (define interface-links (check-interface-links (map (lambda(i) (assoc i links)) extends) parent-impls))
; (define interface-envs (map (lambda(x) (get-interface-env x parent-impls)) interface-links))
; (define cenv (get-rootenv (c-unit-name ast) links))
; (foldr combine-envs cenv (check-empty-interface-envs links cenv interface-envs)
; )


(define (check-heirarchies class-info)
 (map (compose1 (curry check-heriarchy class-info) second) class-info))

;combines two environments by merging in methods and fields. Checks that methods are shadowed properly
(define (combine-envs take-from combine-in)
  (envs-print take-from)
  (define methods (map first (envs-methods take-from)))
  (define method-pairs (map (lambda (x) (list (assoc x (envs-methods combine-in)) (assoc x (envs-methods take-from)))) methods))
  (define (can-shadow? m1 m2)
    (match-let ([(method _ s1 m1 t1 _ _) m1]
                [(method _ s2 m2 t2 _ _) m2])
      (cond
        [(and (equal? s1 'protected) (equal? s2 'public)) (c-errorf "subclass can not lower ~a ~a" s1 s2)]
        [(not (type-ast=? t1 t2)) (c-errorf "return types not equal ~a ~a" t1 t2)]
        [(not (compare-method-modifier-lists m2 m1)) (c-errorf "shadowed methods mods are messed yo")]
        [else #t])))
  
  (define (combine-step par env)
    (match par
      [`(,#f ,x) (env-append env (envs (list (assoc (first x) (envs-types take-from))) empty (list x) empty))]
      [`(,x ,y)  (if (can-shadow? (eval-ast (second x)) (eval-ast (second y))) 
                     env 
                     (c-errorf "Cannot shadow method"))]))

  (define (combine-fields par env)
    (match par
      [`(,key ,value) (env-append env (envs (list (assoc key (envs-types take-from))) (list par) empty empty))]))
  
  (foldr combine-fields (foldr combine-step combine-in method-pairs) (envs-vars take-from)))

;======================================================================================
;==== Error Checking Helpers 
;======================================================================================

(define (check-for-abstract ast env)
  (cond
    [(is-class-with-mod? ast 'abstract) env]
    [(contains-abs-method env) (c-errorf "Can only decalre abstract methods in an abstract class.")]
    [else env]))

(define (contains-abs-method env)
  (define (is-abs? meth)
    (occurs? 'abstract (method-mod meth)))
  (ormap (compose1 is-abs? eval-ast second) (envs-methods env)))

(define occurs? (compose list? member))

(define (compare-method-modifier-lists base-list derived-list)
  (printf "Base ~a~nDerived ~a~n" base-list derived-list)
  (not (or (occurs? 'final base-list)
           (and (occurs? 'static base-list) (not (occurs? 'static derived-list)))
           (and (occurs? 'static derived-list) (not (occurs? 'static base-list))))))

;======================================================================================
;==== Print Functions 
;======================================================================================

(define (print-heir e)
  (printf "CLASS ENV:~n")
  (envs-print e))
