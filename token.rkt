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
                             (lteq "<=") (gteq ">=") (neq "!=") (land "&&") (lor "\\|\\|") (plusplus "++") (minusminus "--")
                             (plus "+") (minus "-") (star "\\*") (slash "/") (amp "&") (bor "\\|") (carot "^") (mod "%")
                             (ltlt "<<") (gtgt ">>") (gtgtgt ">>>") (peq "+=") (minuseq "-=") (stareq "\\*=") (slasheq "/=")
                             (ampeq "&=") (oreq  "\\|=") (caroteq "^=") (modeq "%=") (ltlteq "<<=") (gtgteq ">>=") (gtgtgteq ">>>=")))
(define separators '((oparen "\\(") (cparen "\\)") (ocurl "{") (ccurl "}") (osquare "[") (csquare "]") (semi ";") (comma ",") (dot ".")))
(define literals '((null-lit "null") 
                   (bool-lit "true|false")
                   (decimal-lit "0|(1|2|3|4|5|6|7|8|9)((0|1|2|3|4|5|6|7|8|9)*)")
                   (octal-lit "0((0|1|2|3|4|5|6|7)*)")
                   (hex-lit "0(x|X)((0|1|2|3|4|5|6|7|8|9|a|A|b|B|c|C|d|D|e|E|f|F)*)")))

(define token-exps (append keywords
                           operators
                           separators
                           literals))
token-exps