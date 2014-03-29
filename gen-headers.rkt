#lang racket
(require "generation-structures.rkt")
(require "mangle-names.rkt")

(provide gen-static)

(define (write-info-name out cenv)
  (map (curryr display out) (codeenv-name cenv)))

(define (write-class-array out lst)
  (display "dd " out)
  (display (length lst) out)
  (display "\t; number of entries\n" out)
  (for-each (lambda (x) (for-each (curryr display out)
                                  (list "dd " x "\t; " (string-join x ".") "\n")))
            lst))

(define (gen-static out cenv)
  (define dis-list (compose (curry for-each (curryr display out))))
  (let ([sect-label (apply string-append (codeenv-name cenv))]
        [inter-label "_INTERFACE"])
    
    ;; want to write in a data section
    (dis-list (list "section .data\n\n" "global " sect-label "\n\n"))
    (for-each (lambda (x) (dis-list (list "extern " (mangle-names x) "\n")))
              (filter-not codemeth-ref? (codeenv-methods cenv)))
    (display "\n" out)
    (for-each (lambda (x) (dis-list (list "global " (mangle-names x) "\t; Method Defn\n")))
              (filter codemeth-ref? (codeenv-methods cenv)))
    ;; write the table header so we can find stuff
    (dis-list (list
               "\n"
               ;; label our location so it can be found
               sect-label ":\n"
               "dd " (codeenv-guid cenv) "\t ; the unique id of this class \n"
               "dd " inter-label "\t ; where valid interfaces are declared\n"))
    ;; method pointers go here
    (for-each (lambda (x) (for-each (curryr display out)
                                    (list "dd " (mangle-names x) "\t; scope" "\n" )))
              (reverse (codeenv-methods cenv)))
    
    ;; Static variable points
    (for-each (lambda (x) (dis-list (list "global " (mangle-names x) "\t; Static Var\n"
                                          (mangle-names x) ": dd 0\t; \n")))
              (filter (lambda (x) (and (codevar-ref? x) (codevar-static? x))) (codeenv-vars cenv)))))

