#lang racket
(require "generation-structures.rkt")
(require "mangle-names.rkt")

(provide gen-static)

(define (write-info-name out cenv)
  (map (curryr display out) (codeenv-name cenv)))

(define (write-class-array out lst)
  (display "dw " out)
  (display (length lst) out)
  (display "\t; number of entries\n" out)
  (for-each (lambda (x) (for-each (curryr display out)
                                  (list "dw " x "\t; " (string-join x ".") "\n")))
            lst))

(define (gen-static out cenv)
  (let ([sect-label (apply string-append (codeenv-name cenv))]
        [inter-label "_INTERFACE"])
    
    (for-each (lambda (x) (for-each (curryr display out)
                                    (list "extern " (mangle-names (codemeth-id x)) "\n")))
     (filter-not codemeth-ref? (codeenv-methods cenv)))
    
    ;; want to write in a data section
    (display "\nsection .data\n\n" out)
    ;; write the table header so we can find stuff
    (for-each (curryr display out)
              (list           
               ;; label our location so it can be found
               sect-label ":\n"
               "dw " (codeenv-guid cenv) "\t ; the unique id of this class \n"
               "dw " inter-label "\t ; where valid interfaces are declared\n"))
    ;; method pointers go here
    (for-each (lambda (x) (for-each (curryr display out)
                                    (list "dw " (mangle-names (codemeth-id x)) "\t; scope" "\n" )
                                        
                                        ))
              (reverse (codeenv-methods cenv)))
    
    ;; layout static vars here
    ))

