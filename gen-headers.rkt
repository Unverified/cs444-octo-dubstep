#lang racket
(require "environments.rkt")
(require "generation-structures.rkt")
(require "mangle-names.rkt")

(provide gen-static)

(define (write-info-name out cenv)
  (map (curryr display out) (codeenv-name cenv)))

(define (write-class-array out lst)
  (display "\tdd " out)
  (display (length lst) out)
  (display "\t; number of entries\n" out)
  (for-each (lambda (x) (for-each (curryr display out)
                                  (list "\tdd " x "\t; " (string-join x ".") "\n")))
            lst))

(define (gen-static out all-labels cenv)
  (printf "starting header for ~a~n" (codeenv-name cenv)) 
  (define dis-list (curry for-each (curryr display out)))
  (define my-contrib (map mangle-names (cons cenv (append (filter (lambda (x) (and (codevar-ref? x) (codevar-static? x))) (codeenv-vars cenv))
                                                          (filter codemeth-ref? (codeenv-methods cenv))))))
  
  (for-each (lambda (x) (display (string-append "global " x "\n") out)) my-contrib)
  (display "\n" out)
  (for-each (lambda (x) (display (string-append "extern " x "\n") out)) (filter (compose1 false? (curryr member my-contrib)) all-labels))
  
  
  ;; want to write in a data section
  (display "\n\nsection .data\n" out)
  ;; write the table header so we can find stuff
  (dis-list (list "\n" (mangle-names cenv) ":\n" 
                  "\tdd " (codeenv-guid cenv) "\t ; the unique id of this class \n"))
  ;; method pointers
  (for-each (lambda (meth) (dis-list (list "\tdd " (mangle-names meth) "\t; scope" "\n" )))
            (filter-not (compose1 (curry equal? "") funt-id codemeth-id) 
                        (reverse (codeenv-methods cenv))))
  ;; static variable pointers
  (for-each (lambda (x) (display (string-append (mangle-names x) ": dd 0\t; \n") out))
            (filter (lambda (x) (and (codevar-ref? x) (codevar-static? x))) (codeenv-vars cenv)))
  
  (printf "done header for ~a~n" (codeenv-name cenv)))

