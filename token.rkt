#lang racket
(require "expand-parenthesis.rkt")
(require racket/string)
(provide token-exps)
(provide lookup-regex)
(provide keywords)
(provide operators)
(provide separators)
(provide literals)


(define special-chars (string->list "\\*|~()#"))
(define (char-range f l)
  (map integer->char (range (char->integer f) (add1 (char->integer l)))))



(define all-ascii (map integer->char (range 0 128)))
(define java-char (append (string->list "_$") (char-range #\a #\z) (char-range #\A #\Z)))
(define digits (char-range #\0 #\9))

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
(define literals '((null-lit "null") 
                   (bool-lit "true|false")
                   (decimal-lit "0|(1|2|3|4|5|6|7|8|9)((0|1|2|3|4|5|6|7|8|9)*)")
                   (octal-lit "0((0|1|2|3|4|5|6|7)*)")
		   
                   (floating-point-lit "#(digits).(#(digits)|~)(#(exponent-part)|~)(#(float-type-suffix)|~)|(.#(digits)(#(exponent-part)|~)(#(float-type-suffix)|~))|(#(digits)#(exponent-part)(#(float-type-suffix)|~))|(#(digits)(#(exponent-part)|~)#(float-type-suffix))")
               
		
                   (hex-lit "0(x|X)((0|1|2|3|4|5|6|7|8|9|a|A|b|B|c|C|d|D|e|E|f|F)*)")
                   (char-lit "'(#(char-input-chars)|#(escape-sequence))'")
                   (string-lit "\"(#(string-characters))\"")
                   (comment-lit "#(comment-lit-1)|#(comment-lit-2)")
		   (id "#(java-letter)((#(java-letter)|#(java-digit))*)")))
		   
(define others '(

                   (java-letter "$|_|Q|W|E|R|T|Y|U|I|O|P|A|S|D|F|G|H|J|K|L||Z|X|C|V|B|N|M|q|w|e|r|t|y|u|i|o|p|a|s|d|f|g|h|j|k|l|z|x|c|v|b|n|m")
                   (java-digit "0|1|2|3|4|5|6|7|8|9")
                   (digits "#(java-digit)(#(java-digit)*)")
                   (float-type-suffix "f|F|d|D")
                   (signed-integer "(+|-|~)#(digits)")
		   (octal-digit "0|1|2|3|4|5|6|7")
		   (zero-to-three "0|1|2|3")
		   (escape-sequence "\\\\(b|t|n|f|r|'|\"|\\\\|#(octal-digit)|(#(octal-digit)#(octal-digit))|(#(zero-to-three)#(octal-digit)#(octal-digit)))")
                   (exponent-part "#(exponent-indicator)#(signed-integer)")
                   (exponent-indicator "e|E")
                   (string-characters "((#(escape-sequence)|#(string-input-chars))*)")
                   (comment-lit-1 "//(#(all-non-break-chars)*)(\n|\r)")
                   (comment-lit-2 "/\\*#(no-star)((#(no-star)*)|(((#(no-star)*)(\\*)((\\*)*)#(no-star-no-slash))*))\\*((\\*)*)/")))

                   
                   
(define inputs (list
                   (list 'char-input-chars char-input-char-string)
                   (list 'string-input-chars string-input-char-string)
		   (list 'all-ascii-chars all-ascii-chars)
                   (list 'all-non-break-chars all-non-break-chars)
                   (list 'no-star (all-ascii-remove-and-or-remaining "*"))
                   (list 'no-slash (all-ascii-remove-and-or-remaining "/"))
                   (list 'no-star-no-slash (all-ascii-remove-and-or-remaining "/*"))
                   (list 'line-terminator "\n|\r")))



(define (make-union-regex cl)
  (define (make-escaped-string c)
    (cond
      [(not (char? c)) (error "what are you doing, ya dingus?")]
      [(ormap (curry char=? c) (string->list "()\\|~#")) (string #\\ c)]
      [else (string c)]))
  (string-join (map make-escaped-string cl) "|" #:before-first "(" #:after-last ")"))


(define token-exps-1 (append keywords
                           operators
                           separators
                           literals
			   others
                           inputs))


;;lookup-regex : symbol->string
(define [lookup-regex s]
	((lambda (P)
          (cond
            [(empty? P) (error "Not a valid token name")]
            [else P]))
           
         (filter (lambda (x) (symbol=? s (first x))) token-exps-1)))

;;expand-regex: String -> String
(define [expand-regex STR]
  (define [expand-list LST]
    (cond
      [(empty? LST) empty]
      [(char=? (first LST) #\\) (cons #\\ (cons (first (rest LST)) (expand-list (rest (rest LST)))))]
      [(char=? (first LST) lookup-string)
       ((lambda (P)
          (append (append (cons #\( (expand-list (string->list (first (rest (first (lookup-regex (string->symbol (list->string (first P)))))))))) (cons #\) empty)) (expand-list (first (rest P)))))
        (expand-parenthesis (rest (rest LST)) empty 1))]
      [else (cons (first LST) (expand-list (rest LST)))]))
  (list->string (expand-list (string->list STR))))
  
  
;(expand-regex "#(decimal-lit)|x")
;(expand-regex "#(bool-lit)|#(decimal-lit)")

;(expand-regex "#(char-input-chars)#(string-input-chars)")
;(expand-regex "#(string-lit)")

;(expand-regex "#(char-lit)")
;(expand-regex "#(comment-lit-1)")
;(expand-regex "#(comment-lit-2)")

(define token-exps 
  (map (lambda (x) (cons (first x) (cons (expand-regex (second x)) empty))) token-exps-1))




;(define o-digits (char-range #\0 #\7))
;(define digits (char-range #\0 #\9))
