; Command-line program as an alternative to jqlang.org
; Uncomment for static compilation
(import json-query
        srfi-180
        srfi-13
        (except scheme =)
        (chicken base)
        (chicken port)
        (chicken pretty-print)
        (chicken eval)
        (only srfi-193 command-line))

(eval '(import (except scheme =)
               json-query 
               srfi-1
               srfi-13) 
   (interaction-environment))

(eval '(define-syntax =
        (syntax-rules ()
            ((= first arg ... )
             (cond
                 ((string? first)
                  (string=? first arg ...))
                 (else (eq? first arg ...))))))
    (interaction-environment))

(eval '(define-syntax jq
            (syntax-rules ()
                ((jq arg)
                 (if (list? arg)
                     (json:query arg)
                     (json:query (list arg))))
                ((jq arg ...)
                 (json:query (list arg ...)))))
    (interaction-environment))

(eval '(define-syntax accessor
            (syntax-rules ()
                ((accessor arg ...)
                 (lambda args (json:query `(arg ... ,@args))))))
    (interaction-environment))

; Functions for use in queries
(define prefix string-prefix?)
(define contains string-contains)

(define-record args filter input)

(define (parse-command-line command-line args)
    (cond
        ((string=? (car command-line) "-f")
         (begin
             (args-filter-set! args (cadr command-line))
             (parse-command-line (cddr command-line) args)))
        (else (args-input-set! args (car command-line))))
    args)

(define args (parse-command-line (cdr (command-line))
                                 (make-args "" "")))

(define data 
    (with-input-from-file  (args-input args)
        json-read))

(define filter
    (with-input-from-file (args-filter args)
        read))

(define result 
    ((eval `(jq ,filter) (interaction-environment))
     data))

(if (string? result)
    (display result)
    (pretty-print result))
