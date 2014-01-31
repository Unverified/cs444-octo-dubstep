#lang racket
(provide token-exps)

(define keyword-list '(abstract boolean break byte case catch char class const continue default 
                            do double else extends final finally float for goto if implements import 
                            instanceof int interface long native new package private protected public
                            return short static strictfp super switch synchronized this throw throws
                            transient try void volatile while))

(define [gen-keyword . kw]
  (define [keyword-1 kw]
    (cond
      [(symbol? kw) (list kw (symbol->string kw))]
      [else (error "erros")]))
  (map keyword-1 kw))

(define keywords (apply gen-keyword keyword-list))
(define operators '((eq "=") (gt ">") (lt "<") (bang "!") (tilde "~") (question "?") (colon ":") (eqeq "==")
                             (lteq "<=") (gteq ">=") (neq "!=") (and "&&") (or "||") (plusplus "++") (minusminus "--")
                             (plus "+") (minus "-") (star "*") (slash "/") (amp "&") (bor "|") (carot "^") (mod "%")
                             (ltlt "<<") (gtgt ">>") (gtgtgt ">>>") (peq "+=") (minuseq "-=") (stareq "*=") (slasheq "/=")
                             (ampeq "&=") (oreq  "|=") (caroteq "^=") (modeq "%=") (ltlteq "<<=") (gtgteq ">>=") (gtgtgteq ">>>=")))
(define separators '((oparen "(") (cparen ")") (ocurl "{") (ccurl "}") (osquare "[") (csquare "]") (semi ";") (comma ",") (dot ".")))
(define literals '((null-lit "null") 
                   (bool-lit "true|false")
                   (decimal-lit "")
                   (octal-lit "")
                   (hex-lit "")))

(define token-exps (append keywords
                           operators
                           separators
                           literals))
token-exps