#lang racket
(require "expand-parenthesis.rkt")
(require racket/string)
(provide token-exps)
(provide lookup-regex)
(provide keywords)
(provide operators)
(provide separators)
(provide literals)

(define all-ascii '(#\a #\b))
(define special-chars (string->list "\\*|~()#"))


;(define digits (char-range #\0 #\9))

;;all-ascii-remove-and-or-remaining : string -> string
;;removes all chars in remove-chars from all-ascii and returns a regexp containing the "|" of all remaining characters
(define (all-ascii-remove-and-or-remaining remove-chars)
  (let ([l (filter-not (lambda (x) (member x (string->list remove-chars))) all-ascii)])
    (list->string       
     (append (if (member (first l) special-chars) 
                 (list #\\ (first all-ascii))
                 (list (first all-ascii)))  
             (foldr
              (lambda (x y) (cons #\| (if (member x special-chars)
                                          (cons #\\ (cons x y))
                                          (cons x y))))                     
              empty
              (rest l))))))

;(define char-input-char (filter-not (lambda (x) (member x (string->list "\'\\"))) all-ascii))
;(define char-input-char-string (list->string (append (if (member (first char-input-char) special-chars) (list #\\ (first char-input-char)) (list (first char-input-char))) (foldr (lambda (x y) (cons #\| (if (member x special-chars) (cons #\\ (cons x y)) (cons x y)))) empty (rest char-input-char)))))
(define char-input-char-string (all-ascii-remove-and-or-remaining "\\\'"))
;(define string-input-char (filter-not (lambda (x) (member x (string->list "\"\\"))) all-ascii))
;(define string-input-char-string (list->string (append (if (member (first string-input-char) special-chars) (list #\\ (first string-input-char)) (list (first string-input-char))) (foldr (lambda (x y) (cons #\| (if (member x special-chars) (cons #\\ (cons x y)) (cons x y)))) empty (rest string-input-char)))))
(define string-input-char-string (all-ascii-remove-and-or-remaining "\\\""))

;(define all-ascii-chars (list->string (append (if (member (first all-ascii) special-chars) (list #\\ (first all-ascii)) (list (first all-ascii))) (foldr (lambda (x y) (cons #\| (if (member x special-chars) (cons #\\ (cons x  y)) (cons x y)))) empty (rest all-ascii)))))
(define all-non-break-chars (all-ascii-remove-and-or-remaining "\n\r"))
(define all-ascii-chars (all-ascii-remove-and-or-remaining ""))

(define lookup-string #\#)
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

;(define literals '((null-lit "null") 
;                   (bool-lit "true|false")
;                   (decimal-lit "#(decimal-lit)")
;                   (octal-lit "0(#(octal-digit)*)")
;                   (floating-point-lit "#(digits).(#(digits)|~)(#(exponent-part)|~)(#(float-type-suffix)|~)|(.#(digits)(#(exponent-part)|~)(#(float-type-suffix)|~))|(#(digits)#(exponent-part)(#(float-type-suffix)|~))|(#(digits)(#(exponent-part)|~)#(float-type-suffix))")
;                   (hex-lit "0(x|X)((0|1|2|3|4|5|6|7|8|9|a|A|b|B|c|C|d|D|e|E|f|F)*)")
;                   (char-lit "'(#(char-input-chars)|#(escape-sequence))'")
;                   (string-lit "\"(#(string-characters))\"")
;                   (comment-lit "(#(comment-lit-1)|#(comment-lit-2))")
;                   (id "#(java-letter)((#(java-letter)|#(java-digit))*)")))
		   
;(define others '(            
;                 (signed-integer "(+|-|~)#(digits)")
;                 (escape-sequence "\\\\(b|t|n|f|r|'|\"|\\\\|#(octal-digit)|(#(octal-digit)#(octal-digit))|(#(zero-to-three)#(octal-digit)#(octal-digit)))")
;                 (exponent-part "#(exponent-indicator)#(signed-integer)") 
;                 (string-characters "((#(escape-sequence)|#(string-input-chars))*)")
             
;(define inputs '((list 'char-input-chars char-input-char-string)
;                 (list 'string-input-chars string-input-char-string)
;                 (list 'all-ascii-chars all-ascii-chars)
;                 (list 'all-non-break-chars all-non-break-chars)
;                 (list 'no-star (all-ascii-remove-and-or-remaining "*"))
;                 (list 'no-slash (all-ascii-remove-and-or-remaining "/"))
;                 (list 'no-star-no-slash (all-ascii-remove-and-or-remaining "/*"))
;                 (list 'line-terminator "\n|\r")))

(define literals '((null-lit "null") 
                   (bool-lit "true|false")
                   (decimal-lit "#(decimal-lit)")
                   (octal-lit "0(#(octal-digit)*)")
                   (hex-lit "0(x|X)(#(hex-digit)*)")
                   (char-lit "'(#(char-char)|\\\\(#(reg-escape)|#(oct-escape)))'")
                   (string-lit "\"((#(string-char)|\\\\(#(reg-escape)|#(oct-escape)))*)\"")
                   (comment "//(#(non-break-char)*)")
                   (comment "/\\*((#(no-star)*)(\\*#(no-slash))*)\\*/")
                   (whitespace "#(whitespace)")
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