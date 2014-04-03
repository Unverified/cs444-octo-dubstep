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

(define (public? meth)
  (equal? 'public (method-scope meth)))

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
           [final-env (combine-class-infos cinfo superclass-info interface-infos)])

      (if (empty? extends) (void) (check-constructor-super (info-env superclass-info)))
      
      ((compose1 
        (curryr set-cinfo-ast (gen-top-ast-env (info-env superclass-info) (info-env cinfo) (info-env cinfo) (info-ast cinfo)))
        (curryr set-cinfo-env (check-for-abstract (info-ast cinfo) final-env))
        (curryr set-cinfo-supers (filter-not empty? (info-path superclass-info)))
        (curryr set-cinfo-impls (remove-duplicates (append (info-impls superclass-info) (append-map info-path interface-infos)))))
       cinfo))))

(define (gen-full-interface-info root cinfo subimpls)
  (let* ([implpath (extend-path (info-name cinfo) subimpls)]
         [extends  (get-extends (info-ast cinfo))]
         [interface-infos (check-interfaces-info implpath (map (curry lookup-cunit-env gen-full-interface-info implpath root) extends))])

    ((compose1
     (curryr set-cinfo-env (combine-interface-infos root cinfo interface-infos))
     (curryr set-cinfo-impls 'Interface)
     (curryr set-cinfo-supers (append-map info-path interface-infos)))
     cinfo)))

(define (extend-path newele sub-path)
  (cond [(occurs? newele sub-path) (c-errorf "Cycle detected ~a" (map (curryr string-join ".") (cons newele sub-path)))]
        [else (cons newele sub-path)]))

;(define (check-empty-interface-envs root cenv interface-envs)
;  (cond
;    [(envs? (combine-envs combine-all-meths (info-env (find-info (list "java" "lang" "Object") root)) cenv)) interface-envs]
;    [else interface-envs]))


(define (check-heirarchies class-info)
 (map (curry check-heriarchy class-info) class-info))

; COMBINING METHODS

;gets all abstract methods from info
(define (get-abs-meths info)
  (filter (lambda(x) (abstract? (eval-ast (second x)))) (envs-methods (info-env info))))

;checks if duplicates in take-from and combine-in have the same return type / modifier and adds the one with the lowest scope
(define (combine-abs-meths take-from combine-in)
  (define methods (map first take-from))
  (define method-pairs (map (lambda (x) (list (assoc x combine-in) (assoc x take-from))) methods))

  (define (combine-proc pair all-meths)
    (match pair
      [`(,#f ,t) (cons t all-meths)]
      [`(,c ,t) (cond 
                  [(can-shadow-2? (eval-ast (second c)) (eval-ast (second t)))
                   (if (public? (eval-ast (second t))) (cons t (remove c all-meths)) all-meths)]
                  [else (c-errorf "Cannot shadow method c: ~a, t: ~a" c t)])])) 

  (foldr combine-proc combine-in method-pairs))

(define (combine-envs take-from combine-in)
  (combine-env-meths combine-in (envs-methods take-from) (envs-types take-from)))

;takes all methods in meths and adds them into env, if the meth already exists in env is checks if it can shadow.
(define (combine-env-meths env meths types)
  (define methods (map first meths))
  (define method-pairs (map (lambda (x) (list (assoc x (envs-methods env)) (assoc x meths))) methods))

  (define (combine-proc par env)
    (match par
      [`(,#f ,y) (env-append env (envs (list (assoc (first y) types)) empty (list y) empty))]
      [`(,x ,y) (cond
                  [(can-shadow-1? (eval-ast (second x)) (eval-ast (second y))) env]
                  [else (c-errorf "Cannot shadow method")])]))

  (foldr combine-proc env method-pairs))

;combines a class info with its super and implements infos
(define (combine-class-infos cinfo superinfo implinfos)
  (define (combine-fields types par env)
    (match par
      [`(,key ,value) (env-append env (envs (list (assoc key types)) (list par) empty empty))]))

  ;combine all the non abstract methods in superinfo into class-env, a full shadow check is perform here (ie scope, return type, and modifier are checked)
  (define non-abs-supermeths (filter-not (lambda(x) (abstract? (eval-ast (second x)))) (envs-methods (info-env superinfo))))
  (define class-env (combine-env-meths (info-env cinfo) non-abs-supermeths (envs-types (info-env superinfo))))

  ;get all the abstract methods from superinfo and all the methods from implinfos
  ; - combines them into one list of abstract methods
  ; - if there are duplicate methods then the return type and modifiers are checked and the one with the lowest scope is choosen.
  (define abs-methods (foldl combine-abs-meths (get-abs-meths superinfo) (map get-abs-meths implinfos)))
  (define all-types (append (envs-types (info-env superinfo)) (append-map envs-types (map info-env implinfos))))
  
  ;add the fields, and the abstract methods
  (foldr (curry combine-fields (envs-types (info-env superinfo)))
         (combine-env-meths class-env abs-methods all-types)
         (envs-vars (info-env superinfo))))

(define (combine-interface-infos root iinfo superinfos)
  ;check that there are not shadow conflicts with any Object methods (but dont actually drag them in)
  (combine-envs (info-env (find-info (list "java" "lang" "Object") root)) (info-env iinfo))
  (foldl combine-envs (info-env iinfo) (map info-env superinfos)))

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
