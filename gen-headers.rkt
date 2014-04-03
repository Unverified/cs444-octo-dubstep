#lang racket
(require "environments.rkt")
(require "generation-structures.rkt")
(require "mangle-names.rkt")
(provide gen-static)
(provide cast-off-shift)
(provide write-cast-fields)
(provide get-cast-fields)

(define (cast-off-shift name) 
  (let-values ([(off shf) (quotient/remainder (name->id name) 32)])
    (values (* 4 (+ off 2)) shf)))

(define (write-info-name out cenv)
  (map (curryr display out) (codeenv-name cenv)))

(define (write-class-array out lst)
  (display "\tdd " out)
  (display (length lst) out)
  (display "\t; number of entries\n" out)
  (for-each (lambda (x) (for-each (curryr display out)
                                  (list "\tdd " x "\t; " (string-join x ".") "\n")))
            lst))

(define (write-method-table out off methlst)
  (define (printempty) (display "\tdd 0\n" out))
  (cond
    [(> off (funt->off "LENGTH")) (void)]
    [(empty? methlst) (printempty)(write-method-table out (add1 off) methlst)]
    [(= (* 4 off) (codemeth-off (first methlst))) (display (string-append "\tdd " (mangle-names (first methlst)) "\n") out)
                                                  (write-method-table out (add1 off) (rest methlst))]
    [else (printempty)(write-method-table out (add1 off) methlst)]))

(define (generate-cast-num classes)
  (cond [(empty? classes) 0]
        [else (+ (arithmetic-shift 1 (first classes)) (generate-cast-num (rest classes)))]))

(define (write-cast-fields out iter classids)
  (cond [(> iter (quotient (name->id "length") 32)) 'ok]
        [else (let-values ([(larger smaller) (partition (curry < 32) classids)])
                (fprintf out "\tdd ~a\t; ~b~n" (generate-cast-num smaller) (generate-cast-num smaller))
                (write-cast-fields out (add1 iter) (map (curryr - 32) classids)))]))

(define (get-cast-fields out iter classids)
  (cond [(> iter (quotient (name->id "length") 32)) empty]
        [else (let-values ([(larger smaller) (partition (curry < 32) classids)])
                (cons (generate-cast-num smaller) (get-cast-fields out (add1 iter) (map (curryr - 32) classids))))]))

(define (gen-static out all-labels cenv)
  (printf "starting header for ~a~n" (codeenv-name cenv)) 
  (define dis-list (curry for-each (curryr display out)))
  (define my-contrib (map mangle-names (cons cenv (append (filter (lambda (x) (and (codevar-ref? x) (codevar-static? x))) (codeenv-vars cenv))
                                                          (filter (lambda (x) (and (codemeth-ref? x) (not (codemeth-native? x)))) (codeenv-methods cenv))))))
  
  (for-each (lambda (x) (display (string-append "global " x "\n") out)) 
            (cons (string-append (mangle-names cenv) "METHODTABLE") my-contrib))
  (display "\n" out)
  (for-each (lambda (x) (display (string-append "extern " x "\n") out))
            (filter (compose1 false? (curryr member my-contrib)) all-labels))
  
  ;; want to write in a data section
  (display "\n\nsection .data\n" out)
  ;; write the table header so we can find stuff
  (dis-list (list "\n" (mangle-names cenv) ":\n" 
                  "\tdd " (mangle-names cenv) "METHODTABLE\t ; seletor in the method arrays \n"))
  (dis-list (list "\tdd " (codeenv-guid cenv) "\t; guid\n"))
  (write-cast-fields out 0 (codeenv-casts cenv))
  
  ;; method pointers
  (dis-list (list "\n\n"(mangle-names cenv) "METHODTABLE:\n"))
  (write-method-table out 0 (filter-not (compose1 (curry equal? "") funt-id codemeth-id) 
                                        (reverse (codeenv-methods cenv))))
  
  ;; static variable pointers
  (display "\n" out)
  (for-each (lambda (x) (display (string-append (mangle-names x) ": dd 0\t; \n") out))
            (filter (lambda (x) (and (codevar-ref? x) (codevar-static? x))) (codeenv-vars cenv))) 
  (printf "done header for ~a~n" (codeenv-name cenv)))

