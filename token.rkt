#lang racket
(require "expand-parenthesis.rkt")
(require racket/string)
(provide token-exps)
(provide lookup-regex)
(provide keywords)
(provide operators)
(provide separators)
(provide literals)

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

(define operators '((eq "=") (gt ">") (lt "<") (not "!") (tilde "\\~") (question "?") (colon ":") (eqeq "==")
                             (lteq "<=") (gteq ">=") (noteq "!=") (ampamp "&&") (barbar "\\|\\|") (plusplus "++") (minusminus "--")
                             (plus "+") (minus "-") (star "\\*") (slash "/") (amp "&") (bar "\\|") (carot "^") (pct "%")
                             (ltlt "<<") (gtgt ">>") (gtgtgt ">>>") (pluseq "+=") (minuseq "-=") (stareq "\\*=") (slasheq "/=")
                             (ampeq "&=") (bareq  "\\|=") (caroteq "^=") (pcteq "%=") (ltlteq "<<=") (gtgteq ">>=") (gtgtgteq ">>>=")))

(define separators '((oparen "\\(") (cparen "\\)") (ocurl "{") (ccurl "}") (osquare "[") (csquare "]") (semi ";") (comma ",") (dot ".")))

(define literals '((null-lit "null") 
                   (bool-lit "true|false")
                   (decimal-lit "#(decimal-lit)(l|L|~)")
                   (octal-lit "0(#(octal-digit)*)(l|L|~)")
                   (hex-lit "0(x|X)(#(hex-digit)*)(l|L|~)")
                   (float-lit "#(digits).(#(digits)|~)(#(exponent-indicator)#(signed-int)|~)(#(float-suffix)|~)|.#(digits)(#(exponent-indicator)#(signed-int)|~)(#(float-suffix)|~)|#(digits)#(exponent-indicator)#(signed-int)(#(float-suffix)|~)|#(digits)(#(exponent-indicator)#(signed-int)|~)#(float-suffix)")
                   (char-lit "'(#(char-char)|\\\\(#(reg-escape)|#(oct-escape)))'")
                   (string-lit "\"((#(string-char)|\\\\(#(reg-escape)|#(oct-escape)))*)\"")
                   (comment "//(#(non-break-char)*)")
                   (comment "/((\\*((#(no-star-slash)(/*))*)))((\\*((#(no-star-slash)(/*))*))*)\\*/")
                   (whitespace "#(whitespace)*")
                   (id "#(java-letter)((#(java-letter)|#(java-digit))*)")))

(define token-exps-1 (append keywords
                             operators
                             separators
                             literals))

;;lookup-regex : symbol->string
(define [lookup-regex s]
  ((lambda (P)
     (cond
       [(empty? P) (error "Not a valid token name")]
       [else P]))
   (filter (lambda (x) (symbol=? s (first x))) token-exps-1)))
               
(define token-exps 
  (map (lambda (x) (list (first x) (second x))) token-exps-1))
