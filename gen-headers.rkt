#lang racket
(require "class-info.rkt")
(require "environments.rkt")

(define counter
  (let ([count -1]) (lambda () (set! count (add1 count)) count)))
(define store (make-hash))

(define (name->id name)
  (hash-ref! store name (thunk (counter))))

(define (write-info-name out cinfo)
  (map (curryr display out) (info-name cinfo)))

(define (write-class-array out lst)
  (display "dw " out)
  (display (length lst))
  (display "\t; number of entries\n")
  (for-each (lambda (x) (for-each (curryr display out)
                                  (list "dw " (name->id x) "\t; " (string-join x ".") "\n")))
            lst))

(define (gen-static out cinfo)
  (let ([sect-label (apply string-append (info-name cinfo))]
        [var-label "_STATICV"]
        [inter-label "_INTERFACE"]
        [super-label "_SUPER"])
    ;; want to write in a data section
    (display "section .data\n\n" out)
    ;; write the table header so we can find stuff
    (for-each (curryr display out)
              (list           
               ;; label our location so it can be found
               sect-label ":\n"
               "dw " (name->id (info-name cinfo)) "\t ; the unique id of this class \n"
               "dw " inter-label "\t ; where valid interfaces are declared\n"
               "dw " super-label "\t ; where sub classes are declared\n"))
    ;; method pointers go here
    
    ;; create the interface array
    (display (string-append inter-label ":\n"))
    (write-class-array out (info-impls cinfo))
    ;; create the super types array
    (display (string-append super-label ":\n"))
    (write-class-array out (info-supers cinfo))
    
    
    ;; layout static vars here
    
    ))



(define t (info '("a") empty empty empty '(("B") ("C" "F")) '(("C" "E") ("D"))))
