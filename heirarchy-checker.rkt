#lang racket

(require "types.rkt")
(require "errorf.rkt")
(require "ast-tree.rkt")
(require "class-info.rkt")
(require "environments.rkt")

(provide check-heirarchies)

(define (can-shadow-1? m1 m2)
  (match-let ([(method _ s1 _ _ _ _) m1]
              [(method _ s2 _ _ _ _) m2])
    (cond
      [(and (equal? s1 'protected) (equal? s2 'public)) (c-errorf "subclass can not lower ~a ~a" s1 s2)]
      [else (can-shadow-2? m1 m2)])))

(define (can-shadow-2? m1 m2)
  (match-let ([(method _ _ m1 t1 _ _) m1]
              [(method _ _ m2 t2 _ _) m2])
    (cond
      [(not (type-ast=? t1 t2)) (c-errorf "return types not equal ~a ~a" t1 t2)]
      [(not (compare-method-modifier-lists m2 m1)) (c-errorf "shadowed methods mods are messed yo")]
      [else #t])))

(define (abstract? meth)
  (equal? (list 'abstract) (method-mod meth)))

;======================================================================================
;==== Getter Helpers
;======================================================================================

(define (get-ast-extends ast)
  (define extends (get-extends ast))
  (define java-lang-Object (list "java" "lang" "Object")) 
  (cond
    [(and (empty? extends) (not (equal? (c-unit-name ast) java-lang-Object))) java-lang-Object]
    [else extends]))

;======================================================================================
;==== Heirarchy Checking
;======================================================================================

(define (check-heriarchy root cinfo)
  (cond
    [(is-class? (info-ast cinfo))     (gen-full-class-info root cinfo empty)]
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
        [(empty? (info-ast cinfo)) (info empty empty env-empty empty empty empty)]
        [else (let ([name (c-unit-name (info-ast cinfo))])
                (cond [(not (is-class? (info-ast cinfo))) (c-errorf "Must extend a class.")]
                      [(is-class-with-mod? (info-ast cinfo) 'final) (c-errorf "Cannot extend a final class.")]
                      [else cinfo]))]))

(define (lookup-cunit-env F subs root fullname)
  (cond [(empty? fullname) (info empty empty env-empty empty empty empty)]
        [else (match (find-info fullname root)
                [#f (error "full name not found, how did i get here?")]
                [cinfo (F root cinfo subs)])]))

(define (check-constructor-super super-envs)
  (define super-cons (map first (envs-constructors super-envs)))
  (define empty-arg-cons (funt "" empty))
  (cond
    [(false? (member empty-arg-cons super-cons)) (c-errorf "Extends class must contain a zero argument constructor")]
    [else #f]))

(define (gen-full-class-info root cinfo subclasses)
  (let ([classpath (extend-path (info-name cinfo) subclasses)]
        [extends (get-ast-extends (info-ast cinfo))]
        [implements (get-implements (info-ast cinfo))])
    (let* ([superclass-info (check-class-info classpath (lookup-cunit-env gen-full-class-info classpath root extends))]
           [interface-infos (check-interfaces-info empty (map (curry lookup-cunit-env gen-full-interface-info empty root) implements))]
           [linked-envs (foldl (curry combine-envs combine-impl-meths) env-empty (map info-env interface-infos))]
           [cenv (combine-envs combine-impl-meths linked-envs (combine-envs combine-all-meths (info-env superclass-info) (info-env cinfo))
                               )])

      (if (empty? extends) (void) (check-constructor-super (info-env superclass-info)))
      
      ((compose1 
        (curryr set-cinfo-ast (gen-top-ast-env (info-env superclass-info) (info-env cinfo) (info-env cinfo) (info-ast cinfo)))
        (curryr set-cinfo-env (check-for-abstract (info-ast cinfo) cenv))
        (curryr set-cinfo-supers (filter-not empty? (info-path superclass-info)))
        (curryr set-cinfo-impls (remove-duplicates (append (info-impls superclass-info) (append-map info-path interface-infos)))))
       cinfo))))

(define (gen-full-interface-info root cinfo subimpls)
  (let ([implpath (extend-path (info-name cinfo) subimpls)]
        [extends  (get-extends (info-ast cinfo))])
    (define interface-infos (check-interfaces-info implpath (map (curry lookup-cunit-env gen-full-interface-info implpath root) extends)))

    ((compose1
     (curryr set-cinfo-env (foldr (curry combine-envs combine-all-meths) (info-env cinfo) (check-empty-interface-envs root (info-env cinfo) (map info-env interface-infos))))
     (curryr set-cinfo-impls 'Interface)
     (curryr set-cinfo-supers (append-map info-path interface-infos)))
     cinfo)))

(define (extend-path newele sub-path)
  (cond [(occurs? newele sub-path) (c-errorf "Cycle detected ~a" (map (curryr string-join ".") (cons newele sub-path)))]
        [else (cons newele sub-path)]))

(define (check-empty-interface-envs root cenv interface-envs)
  (cond
    [(envs? (combine-envs combine-all-meths (info-env (find-info (list "java" "lang" "Object") root)) cenv)) interface-envs]
    [else interface-envs]))


(define (check-heirarchies class-info)
 (map (curry check-heriarchy class-info) class-info))

(define (combine-all-meths take-from par env)
  (match par
    [`(,#f ,x) (env-append env (envs (list (assoc (first x) (envs-types take-from))) empty (list x) empty))]
    [`(,x ,y) (cond
                [(can-shadow-1? (eval-ast (second x)) (eval-ast (second y))) env]
                [else (c-errorf "Cannot shadow method")])]))

(define (combine-impl-meths take-from par env)
  (match par
    [`(,#f ,x) (env-append env (envs (list (assoc (first x) (envs-types take-from))) empty (list x) empty))]
    [`(,c ,impl) (let ([c-ast (eval-ast (second c))]
                       [impl-ast (eval-ast (second impl))]
                       [p (assoc (first impl) (envs-types take-from))])
                   (cond
                     [(and (abstract? c-ast) (abstract? impl-ast) (can-shadow-2? c-ast impl-ast))
                      (envs (cons p (remove p (envs-types env)))
                            (envs-vars env)
                            (cons impl (remove impl (envs-methods env)))
                            (envs-constructors env))]
                     [else (combine-all-meths take-from par env)]))]))

(define (combine-fields take-from par env)
  (match par
    [`(,key ,value) (env-append env (envs (list (assoc key (envs-types take-from))) (list par) empty empty))]))

;combines two environments by merging in methods and fields. Checks that methods are shadowed properly
(define (combine-envs combmeth-proc take-from combine-in)
  (define methods (map first (envs-methods take-from)))
  (define method-pairs (map (lambda (x) (list (assoc x (envs-methods combine-in)) (assoc x (envs-methods take-from)))) methods))
  
  (foldr (curry combine-fields take-from) 
         (foldr (curry combmeth-proc take-from) 
                combine-in 
                method-pairs) 
         (envs-vars take-from)))

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
  (not (or (occurs? 'final base-list)
           (and (occurs? 'static base-list) (not (occurs? 'static derived-list)))
           (and (occurs? 'static derived-list) (not (occurs? 'static base-list))))))

;======================================================================================
;==== Print Functions 
;======================================================================================

(define (print-heir e)
  (printf "CLASS ENV:~n")
  (envs-print e))
