#!r6rs
(load "./util.sls")
(library (json-query) 
     (export json:query
             json:ref
             json:keys
             json:values
             json:flatten
             json:unique
             json:replace)
     (import (rnrs)
             (srfi-180)
             (only (srfi :1) delete-duplicates)
             (only (chezscheme) format eval vector-append)
             (util))

     (define (tree-map func)
         (lambda (tree)
             (cond
                ((null? tree)
                 '())
                ((vector? tree)
                 (vector-map (tree-map func) tree))
                ((pair? tree)
                 (cons ((tree-map func) (car tree))
                       ((tree-map func) (cdr tree))))
                (else (func tree)))))

     (define (json:ref key)
         (let ((key (if (string? key) 
                        (string->symbol key)
                        key)))
             (lambda (node)
                 (cdr (assq key node)))))
     
     (define (json:keys node)
         (list->vector (map car node)))
         
     (define (json:values node)
         (list->vector (map cdr node)))

     (define (json:flatten nodes)
         (apply vector-append (vector->list nodes)))
         
     (define (json:unique nodes)
         (-> nodes
             vector->list
             delete-duplicates
             list->vector))

     (define (json:replace new-node)
         (lambda (node) new-node))

     (define (execute-procedures tree node)
         ; Execute all procedures found anywhere within the node
        ((tree-map 
            (lambda (obj) 
                (if (procedure? obj)
                    (obj node)
                    obj)))
         tree))

     (define (to-json-function func)
        (-> func
            symbol->string
            (lambda (x) (string-append "json:" x))
            string->symbol
            eval))

     (define (interpret-function-rule rule)
         (if (procedure? rule) rule
         (let ((func (car rule))
               (args (cdr rule)))
             (cond
                ((eq? func '*)
                 ;input is a vector of nodes instead of just a single node
                 (lambda (nodes) (vector-map (json:query args) nodes)))
                ((eq? func 'filter)
                 (lambda (nodes) 
                     (vector-filter 
                          (lambda (node)
                            (eval (execute-procedures (car args)
                                                      node)))
                          nodes)))
                (else
                 ; For replace
                 (lambda (node) ((apply (to-json-function func)
                                        (execute-procedures args node))
                                 node)))))))

     (define (interpret-rule rule)
        (cond
            ((string? rule)
             (json:ref rule))
            ((procedure? rule)
             rule)
            ((symbol? rule)
             (to-json-function rule))
            ((list? rule)
             (interpret-function-rule rule))
            (else (error 'inperpret-rule "Incorrect rule" rule))))
         
     (define (json:query rules)
         (lambda (node) 
            (apply -> node (map interpret-rule rules))))
         
)
