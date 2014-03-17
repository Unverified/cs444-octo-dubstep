#lang racket

(require "types.rkt")
(require "errorf.rkt")
(require "ast-tree.rkt")
(require "class-info.rkt")

(provide reachability)

(define maybe #t)
(define no #f)

(define (reachability cinfo)
  (printf "########## CHECKING REACHABILITY FOR ~a ############~n" (info-name cinfo))
  (define (reach-statements statements in last-out)
    (cond
      [(empty? statements) last-out]
      [(not last-out) (c-errorf "Unreachable statement: ~a" (first statements))]
      [else (reach-statements (rest statements) in (reach last-out (first statements)))]))

  (define (reach in t)
    (match t
      [(cunit package _ (class _ _ _ _ _ _ bdy)) (for-each (curry reach in) (block-statements bdy))]

      [(constructor _ _ md bdy) (reach maybe bdy)]

      [(method _ _ (not (or '(abstract) '(static native))) t md bdy) 
        (cond 
          [(and (reach maybe bdy) (not (type-ast=? t (ptype 'void)))) 
           (c-errorf "Method: \"~a\" in class: \"~a\" does not have a return statement." (methoddecl-id md) (info-name cinfo))]
          [else no])]

      [(iff _ test tru '()) (reach in tru) in]
      [(iff _ test tru fls) (or (reach in tru) (reach in fls))]

      [(or (while _ (literal _ (ptype 'boolean) "true") bdy)
           (for _ _ (literal _ (ptype 'boolean) "true") _ bdy)) (reach in bdy) no]
      [(or (while _ (literal _ (ptype 'boolean) "false") bdy)
           (for _ _ (literal _ (ptype 'boolean) "false") _ bdy)) (c-errorf "Unreachable code in while(false)/for(;false;).")]
      [(or (while _ _ bdy)
           (for _ _ _ _ bdy)) (reach in bdy) in]

      [(return _ _) no]

      [(block _ _ statements) (reach-statements statements in in)]

      [_ in]))

  (reach maybe (info-ast cinfo)))
