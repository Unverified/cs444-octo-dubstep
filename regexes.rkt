#lang racket
(require "token.rkt")
(require "machine.rkt")
(require "expand-parenthesis.rkt")

(provide string->machine)


(define (char-range f l)
  (map integer->char (range (char->integer f) (add1 (char->integer l)))))

(define (make-union-regex cl)
  (define (make-escaped-string c)
    (cond
      [(not (char? c)) (error "what are you doing, ya dingus? chars only plox")]
      [(ormap (curry char=? c) (string->list "()\\|~#")) (string #\\ c)]
      [else (string c)]))
  (string-join (map make-escaped-string cl) "|" #:before-first "(" #:after-last ")"))

(define (m-char-union cl)
  (let ([start (gensym)]
        [final (gensym)])
    (machine (list start final)
             start
             (list final)
             (map (lambda (x) (transition start x final)) cl)
             empty)))

(define blocks
  (let* ([get-machine (lambda (x list) (copy-machine (second (assoc x list))))]
         [union-blocks (map (lambda (x) (list (first x) (m-char-union (second x))))
                          `((java-digit ,(char-range #\0 #\9))
                            (octal-digit ,(char-range #\0 #\7))
                            (java-letter ,(append (string->list "_$") (char-range #\a #\z) (char-range #\A #\Z)))
                            (one-to-nine ,(char-range #\1 #\9))
                            (zero-to-three ,(char-range #\0 #\3))
                            (exponent-indicator ,(string->list "eE"))
                            (float-suffix  ,(string->list "fFdD"))
                            (reg-escape ,(string->list "btnfr'\"\\"))
                            (hex-digit ,(string->list "0123456789aAbBcCdDeEfF"))
                            (string-char ,(filter-not (lambda (x) (member x (string->list "\\\""))) (map integer->char (range 0 128))))
                            (char-char ,(filter-not (lambda (x) (member x (string->list "\\'"))) (map integer->char (range 0 128))))
                            (non-break-char ,(filter-not (lambda (x) (member x (string->list "\n\r"))) (map integer->char (range 0 128))))
                            (no-star ,(filter-not (lambda (x) (member x (string->list "*"))) (map integer->char (range 0 128))))
                            (no-slash ,(filter-not (lambda (x) (member x (string->list "/"))) (map integer->char (range 0 128))))
                            (no-star-slash ,(filter-not (lambda (x) (member x (string->list "*/"))) (map integer->char (range 0 128))))
                            (whitespace ,(string->list "\t\n\r\f "))
                            (sign ,(string->list "+-"))
                            ))]
         [composed-blocks `((digits ,(copy-machine (opt (concat (list (get-machine 'java-digit union-blocks) 
                                                                      (kleene-star  (get-machine 'java-digit union-blocks)))))))
                            (oct-escape ,(copy-machine (opt (union (list (get-machine 'octal-digit union-blocks)
                                                                         (concat (list (get-machine 'octal-digit union-blocks) (get-machine 'octal-digit union-blocks)))
                                                                         (concat (list (get-machine 'zero-to-three union-blocks) (get-machine 'octal-digit union-blocks) (get-machine 'octal-digit union-blocks))))))))                                    
                            (decimal-lit ,(copy-machine (opt (union (list (m-char-union (list #\0))
                                                                          (concat (list (get-machine 'one-to-nine union-blocks) (kleene-star (get-machine 'java-digit union-blocks)))))))))
                            )]
         [top-blocks `((signed-int ,(copy-machine (opt (concat (list (union (list (get-machine 'sign union-blocks) (m-only-epsilon)))
                                                                     (get-machine 'digits composed-blocks))))))
                       )])
  (append top-blocks union-blocks composed-blocks)))


;;(struct : empty-regex ())
(struct empty-regex ())
;;( struct : concatenation ([left : (union concatenation Char k-star alternation)] [right : (Union concatenation Char k-star alternation)]))
(struct concatenation (left right) #:transparent)
;;( struct : alternation ([options (Listof (union concatenation Char k-star alternation))]))
(struct alternation (options) #:transparent)
;;( struct : k-star ([body (union concatenation Char k-star alternation)]))
(struct k-star (body) #:transparent)
;( struct : block ([id symbol]))
(struct block (id)    #:transparent)
;( struct : literal ([literal Char])) )
(struct literal (char) #:transparent)
  
;;(: regex->machine  : (union concatenation Char k-star alternation) -> machine)
(define [regex->machine R]
  (cond
    [(empty-regex? R) (m-only-epsilon)]
    [(char? R) (m-single-char R)]
    [(concatenation? R) (concat (list (regex->machine (concatenation-left R))
                                      (regex->machine (concatenation-right R))))]
    [(alternation? R) (union (map regex->machine (alternation-options R)))]
    [(k-star? R) (kleene-star (regex->machine (k-star-body R)))]
    [(block? R) (second (assoc (block-id R) blocks))]
    [else (error "Not a regular expression")]))

;;(: list->regex : (Listof Char) (union concatenation Char k-star alternation) ->blocks (union concatenation Char k-star alternation))
(define (list->regex lst R)
  (match lst
    [`() R]
    [`(#\\ ,x ,y ...) (list->regex y (concatenation R x))]
    [`(#\( ,x ...) 
     ((lambda (P)
        ((lambda (Q)
           (list->regex (first (rest P)) Q))
         (concatenation R (list->regex (first P) (empty-regex)))))
        (expand-parenthesis x empty 1))]
    [`(#\* ,x ...) 
     (concatenation (k-star R) (list->regex x (empty-regex)))]
    [`(#\| ,x ...)
     ((lambda (S)
       (cond
         ;[(empty-regex? S) (cons R empty)]
         [(alternation? S) (alternation (cons R (alternation-options S)))]
         [else (alternation (cons R (cons S empty)))]))
      (list->regex x (empty-regex)))]
    [`(#\# #\( ,x ...)
     ((lambda (p)
        ((lambda (q) 
           (list->regex (first (rest p)) q))
          (concatenation R (block (string->symbol (list->string (first p)))))))
      (expand-parenthesis x empty 1))]
    [`(#\~ ,x ...) (list->regex x (concatenation R (empty-regex)))] 
    [_ (list->regex (rest lst) (concatenation R (first lst)))]))

(define (parse str)
  (define (make-sexpr chrs ccp)
    (match chrs
      [`(OPARN ,x ...)  empty]
      [`(CPARN ,x ...)  empty]
      [x (cons (first x) (make-sexpr (rest x)))]
    ))
  
  (make-sexpr (string->list str)))

(define (tokenize lst)
  (match lst
    [`(#\| ,x ...)     (cons 'ALT (tokenize x))]
    [`(#\* ,x ...)     (cons 'STAR (tokenize x))]
    [`(#\~ ,x ...)     (cons 'TILD (tokenize x))]
    [`(#\# #\( ,x ...) (cons 'OHPRN (tokenize x))]
    [`(#\( ,x ...)     (cons 'OPARN (tokenize x))]
    [`(#\) ,x ...)     (cons 'CPARN (tokenize x))]
    [`(#\\ ,x ,y ...)  (cons (literal x) (tokenize y))]
    [x  (if (empty? x)
            empty
            (cons (literal (first x)) (tokenize (rest x))))]))



(define (sreg->machine str)
  (regex->machine (list->regex (string->list str) (empty-regex))))

;(tokenize (string->list "a|b|(ac)"))

;(print-machine (copy-machine (nfa->dfa (regex->machine (list->regex (string->list "grey") (empty-regex))))))
;(expand-parenthesis (string->list "a|e)y") empty 1)
;(print-machine (copy-machine (nfa->dfa (regex->machine (list->regex (string->list "gr(a|e)y") (empty-regex))))))

;(print-machine (copy-machine (nfa->dfa (regex->machine (list->regex (string->list "a*") (empty-regex))))))

(define (string->machine S token-name)
  (let ([machine-1 (regex->machine (list->regex (string->list S) (empty-regex)))])
    (machine 
          (machine-states machine-1)
          (machine-start machine-1)
          (machine-accepting machine-1)
          (machine-transitions machine-1)
          (map (lambda (x) (list x token-name)) (machine-accepting machine-1)))))