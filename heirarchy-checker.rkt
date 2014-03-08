#lang racket

(require "environments.rkt")
(require "type-linker.rkt")
(require "ast-tree.rkt")
	
(provide check-heirarchies)

(provide type-ast=?)

(define (type-ast=? links t1 t2)
  (printf "type-ast=? ~a ~a~n" t1 t2)
  (match (list t1 t2)
    [`(void void) #t]	;hack this bitch out
    [`(,(ptype _ ta) ,(ptype _ tb)) (equal? ta tb)]
    [`(,(atype _ ta) ,(atype _ tb)) (type-ast=? links ta tb)]
    [`(,(rtype _ ta) ,(rtype _ tb)) (type-ast=? links ta tb)]
    [`((,ta ...) (,tb ...)) (printf "type-ast2=? ~a ~a~n" ta tb) (printf "~a : ~a~n" (get-full ta links) (get-full tb links)) (equal? (get-full ta links) (get-full tb links))]
    [_ #f]))

;======================================================================================
;==== Getter Helpers
;======================================================================================

(define (debug-second message x)
  (printf "~a~n" message)
  (second x))

(define (get-ast-extends ast)
  (define extends (get-extends ast))
  (define java-lang-Object (list "java" "lang" "Object")) 
  (cond
    [(and (empty? extends) (not (equal? (c-unit-name ast) java-lang-Object))) java-lang-Object]
    [else extends]))

(define (get-env typename links)
  (roote-env (link-env (debug-second "&&&&&& Here 1" (assoc typename links)))))

(define (get-full typename links)
  (define l (assoc typename links))
  (cond
    [(false? l) (list "java" "lang" (first typename))]	;SUPER hacky, need to change this mister Nick
    [else (link-full (debug-second "&&&&&& Here 2" (assoc typename links)))]))

(define (get-linked-ast l asts)
  (define rootenv (link-env (debug-second "&&&&&& Here 3" l)))
  (define id (roote-id rootenv))
  (debug-second "&&&&&& Here 4" (assoc id asts)))
  
(define (get-linked-links l all-links)
  (define rootenv (link-env (debug-second "&&&&&& Here 5" l)))
  (define id (roote-id rootenv))
  (debug-second "&&&&&& Here 6" (assoc id all-links)))

;======================================================================================
;==== Heirarchy Checking
;======================================================================================

(define (check-heirarchies asts all-links)

  ;check an ast for proper heirarchy
  (define (check-heirarchy ast links)
    (cond
      [(is-class ast) (get-class-heriarchy ast links empty)]
      [(is-interface ast) (get-interface-heriarchy ast links empty)]))

  ;check a class for proper heirarchy
  (define (get-class-heriarchy ast links extds)
    (printf "CHECKING CLASS HEIR FOR: ~a~n" (c-unit-name ast))
    (define parent-extds (cons (c-unit-name ast) extds))
    (define extends (get-ast-extends ast))
    (define implements (get-implements ast))
    (printf "--- EXTENDS: ~a~n" extends)
    (printf "--- IMPLEMENTS: ~a~n" implements)
    (define class-link (check-class-link (assoc extends links) parent-extds))
    (define interface-links (check-interface-links (map (lambda(i) (assoc i links)) implements) empty))

    ; "DO STUFF HERE"    
    (define extends-env (get-extends-env class-link parent-extds))
    (define interface-envs (map (lambda(x) (get-interface-env x empty)) interface-links))

    (define cur-class-env (combine-envs links extends-env (get-env (c-unit-name ast) links)))
    (define return-env (foldr (curry combine-envs links) cur-class-env interface-envs))
    (check-for-abstract ast return-env))

  ;check an interface for proper heirarchy
  (define (get-interface-heriarchy ast links impls)
    (printf "CHECKING INTERFACE HEIR FOR: ~a~n" (c-unit-name ast))
    (define parent-impls (cons (c-unit-name ast) impls))
    (define extends (get-extends ast))
    (printf "--- EXTENDS: ~a~n" extends)
    (define interface-links (check-interface-links (map (lambda(i) (assoc i links)) extends) parent-impls))
    (define interface-envs (map (lambda(x) (get-interface-env x parent-impls)) interface-links))

    (define cenv (get-env (c-unit-name ast) links))

    (foldr (curry combine-envs links) cenv (check-empty-interface-envs links cenv interface-envs)))

  ;get the heirarchacle environment for an extends class
  (define (get-extends-env class-link parent-extds)
    (cond
      [(false? class-link) env-empty]
      [else (get-class-heriarchy (get-linked-ast class-link asts) (get-linked-links class-link all-links) parent-extds)]))

  ;get the heirarchacle environment for an implements interface
  (define (get-interface-env interface-link parent-impls)
    (cond
     [(false? interface-link) env-empty]
      [else (get-interface-heriarchy (get-linked-ast interface-link asts) (get-linked-links interface-link all-links) parent-impls)]))

  ;check that l links to a class and that it does not exist in parent-extds
  (define (check-class-link l parent-extds)
    (cond
      [(false? l) l]
      [(not (is-class (get-linked-ast l asts))) (error "Must extend a class.")]
      [(is-class-with-mod (get-linked-ast l asts) 'final) (error "Cannot extend a final class.")]
      [else (check-for-duplication l parent-extds)]))

  ;check that l links to an interface and that it does not exist in seen-so-far
  (define (check-interface-links ls seen-so-far)
    (define (check-interface-link l)
      (cond
        [(false? l) l]
        [(not (is-interface (get-linked-ast l asts))) (error "Must implement an interface.")]
        [else (check-for-duplication l seen-so-far)]))
    (cond
      [(empty? ls) empty]
      [else (cons (check-interface-link (first ls)) (check-interface-links (rest ls) (cons (link-full (debug-second "&&&&&& Here 7" (first ls))) seen-so-far)))]))

  ;loop through each ast and check the heirarchy for it
  (map (lambda(ast links) (printf "====== CHECKING HEIRARCHY FOR AST, class/interface: ~a ======~n" 
                          (c-unit-name (debug-second "&&&&&& Here 8" ast))) (check-heirarchy (debug-second "&&&&&& Here 9" ast) (debug-second "&&&&&& Here 10" links))) asts all-links))


(define (check-empty-interface-envs links cenv interface-envs)
  (printf "HERE NICK~n")
  (cond
    [(envs? (combine-envs links (get-env (list "java" "lang" "Object") links) cenv)) interface-envs]
    [else interface-envs]))

;combines two environments by merging in methods and fields. Checks that methods are shadowed properly
(define (combine-envs links take-from combine-in)
  (envs-print take-from)
  (define methods (map first (envs-methods take-from)))
  (define method-pairs (map (lambda (x) (list (assoc x (envs-methods combine-in)) (assoc x (envs-methods take-from)))) methods))
  (define (can-shadow? m1 m2)
    (printf "HEEEERRRRREEERRRRR:~n~a~n~a~n" m1 m2)
    (match-let ([(method _ s1 m1 t1 _ _) m1]
                [(method _ s2 m2 t2 _ _) m2])
      (printf "can-shadow? ~a ~a~n" s1 s2)
      (cond
        [(and (equal? s1 'protected) (equal? s2 'public)) (error "subclass can not lower" s1 s2)]
        [(not (type-ast=? links t1 t2)) (error "return types not equal" t1 t2)]
        [(not (compare-method-modifier-lists m2 m1)) (error "shadowed methods mods are messed yo")]
        [else #t])))
  
  (define (combine-step par env)
    (match par
      [`(,#f ,x) (env-append env (envs (list (assoc (first x) (envs-types take-from))) empty (list x) empty))]
      [`(,x ,y)  (if (can-shadow? (eval-ast (debug-second "&&&&&& Here 12" x)) (eval-ast (debug-second "&&&&&& Here 13" y))) env (error))]))

  (define (combine-fields par env)
    (match par
      [`(,key ,value) (env-append env (envs (list (assoc key (envs-types take-from))) (list par) empty empty))]))
  
  (foldr combine-fields (foldr combine-step combine-in method-pairs) (envs-vars take-from)))

;======================================================================================
;==== Error Checking Helpers 
;======================================================================================

(define (check-for-abstract ast env)
  (cond
    [(is-class-with-mod ast 'abstract) env]
    [(contains-abs-method env) (printf "PROBLEM ENV~n") (envs-print env) (error "Can only decalre abstract methods in an abstract class.")]
    [else env]))

(define (contains-abs-method env)
  (define (is-abs? m)
    (match-let ([(method _ _ m s b _) m])
      (cond
        [(list? (member 'abstract m)) (printf "HERHRHRHRHRHRH: ~a~n"b) #t]
        [else #f])))

  (ormap (lambda(x) (is-abs? (eval-ast (debug-second "&&&&&& Here 14" x)))) (envs-methods env)))

(define (check-for-duplication l parents)
  (cond 
    [(list? (member (link-full (debug-second "&&&&&& Here 15" l)) parents)) (printf "Loop in extends or duplicate implements detected.~n") (error)]
    [else l]))

(define (compare-method-modifier-lists base-list derived-list)
  (printf "compare-method-modifier-lists: ~a ~a~n" base-list derived-list)
  (cond
    [(and (list? (member 'static base-list)) (not (list? (member 'static derived-list)))) #f]
    [(and (list? (member 'static derived-list)) (not (list? (member 'static base-list)))) #f]
    [(list? (member 'final base-list)) #f]
    [else #t]))


(define (scope>? s1 s2)
  (define (get-scope-val scope)
    (define scope-order (list 'public 'protected 'private))
    (length (takef-right scope-order (curry symbol=? scope))))
  (> (get-scope-val s1) (get-scope-val s2)))

;======================================================================================
;==== Print Functions 
;======================================================================================

(define (print-heir e)
  (printf "CLASS ENV:~n")
  (envs-print e))

;======================================================================================
;==== Error 
;======================================================================================

(define (error . x)
  (printf "Error: ~a~n" x)
  (exit 42))
