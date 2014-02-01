#lang racket
(require racket/string)
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
(define operators '((eq "=") (gt ">") (lt "<") (not "!") (tilde "\\~") (question "?") (colon ":") (eqeq "==")
                             (lteq "<=") (gteq ">=") (noteq "!=") (ampamp "&&") (barbar "\\|\\|") (plusplus "++") (minusminus "--")
                             (plus "+") (minus "-") (star "\\*") (slash "/") (amp "&") (bar "\\|") (carot "^") (pct "%")
                             (ltlt "<<") (gtgt ">>") (gtgtgt ">>>") (pluseq "+=") (minuseq "-=") (stareq "\\*=") (slasheq "/=")
                             (ampeq "&=") (bareq  "\\|=") (caroteq "^=") (pcteq "%=") (ltlteq "<<=") (gtgteq ">>=") (gtgtgteq ">>>=")))
(define separators '((oparen "\\(") (cparen "\\)") (ocurl "{") (ccurl "}") (osquare "[") (csquare "]") (semi ";") (comma ",") (dot ".")))
(define literals '((null-lit "null") 
                   (bool-lit "true|false")
                   (decimal-lit "0|(1|2|3|4|5|6|7|8|9)((0|1|2|3|4|5|6|7|8|9)*)")
                   (octal-lit "0((0|1|2|3|4|5|6|7)*)")
                   (hex-lit "0(x|X)((0|1|2|3|4|5|6|7|8|9|a|A|b|B|c|C|d|D|e|E|f|F)*)")))
		   ;(floating-point-lit "((0|1|2|3|4|5|6|7|8|9)((0|1|2|3|4|5|6|7|8|9)*).((0|1|2|3|4|5|6|7|8|9)*)(((E|e)(+|-|~)(0|1|2|3|4|5|6|7|8|9)((0|1|2|3|4|5|6|7|8|9)*))|~)(F|f|D|d|~))|((0|1|2|3|4|5|6|7|8|9)((0|1|2|3|4|5|6|7|8|9)*)(e|E)(+|-|~)(0|1|2|3|4|5|6|7|8|9)((0|1|2|3|4|5|6|7|8|9)*)(F|f|d|D|~))|(.(0|1|2|3|4|5|6|7|8|9)((0|1|2|3|4|5|6|7|8|9)*)(((E|e)(+|-|~)(0|1|2|3|4|5|6|7|8|9)((0|1|2|3|4|5|6|7|8|9)*))|~)(F|f|D|d|~))|((0|1|2|3|4|5|6|7|8|9)((0|1|2|3|4|5|6|7|8|9)*)(((E|e)(+|-|~)(0|1|2|3|4|5|6|7|8|9)((0|1|2|3|4|5|6|7|8|9)*))|~)(F|f|D|d))")

(define (char-range f l)
  (map integer->char (range (char->integer f) (add1 (char->integer l)))))

(define (make-union-regex cl)
  (define (make-escaped-string c)
    (cond
      [(not (char? c)) (error "what are you doing, ya dingus?")]
      [(ormap (curry char=? c) (string->list "()\\|~#")) (string #\\ c)]
      [else (string c)]))
  (string-join (map make-escaped-string cl) "|" #:before-first "(" #:after-last ")"))

(define all-ascii (map integer->char (range 0 128)))
(define java-char (append (string->list "_$") (char-range #\a #\z) (char-range #\A #\Z)))
(define digits (char-range #\0 #\9))

(define token-exps (append keywords
                           operators
                           separators
                           literals))
token-exps
