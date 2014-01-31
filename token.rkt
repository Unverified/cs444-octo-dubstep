#lang racket
(provide token-exps)

(define keyword-list '(abstract boolean break byte case catch char class const continue default 
                            do double else extends final finally float for goto if implements import 
                            instanceof int interface long native new package private protected public
                            return short static strictfp super switch synchronized this throw throws
                            transient try void volatile while))
(define operators '(( "=")
                    (gt ">")
                    (lt "<")
                    (bang "!")
                    (tilde "~")
                    (question "?")
                    (colon ":")
                    (eq "==")
                    ( "<=")
                    ( ">=")
                    ( "!=")
                    ( "&&")
                    ( "||")
                    ( "++")
                    ( "--")
                    ( "+")
                    ( "-")
                    ( "*")
                    ( "/")
                    ( "&")
                    ( "|")
                    ( "^")
                    ( "%")
                    ( "<<")
                    ( ">>")
                    ( ">>>")
                    ( "+=")
                    ( "-=")
                    ( "*=")
                    ( "/=")
                    ( "&=")
                    ( "|=")
                    ( "^=")
                    ( "%=")
                    ( "<<=")
                    ( ">>=")
                    ( ">>>=")))


(define [gen-keyword . kw]
  (define [keyword-1 kw]
    (cond
      [(symbol? kw) (list kw (symbol->string kw))]
      [else (error "erros")]))
  (map keyword-1 kw))

(define keywords (apply gen-keyword keyword-list))


(define token-exps (append keywords
                           operators
                           (list '(null-lit "null"))))

token-exps