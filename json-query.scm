; Uncomment for static compilation
;(declare (unit json-query))
;(declare (uses util))
(module (json-query) (json:query
                      json:ref
                      json:keys
                      json:values
                      json:flatten
                      json:unique
                      json:replace
                      json:write)
     (import scheme
             (chicken port)
             (chicken base)
             (chicken eval)
             (chicken module)
             (only srfi-180 json-write)
             (only vector-lib vector-map vector-append)
             (only srfi-1 delete-duplicates)
             util)

    (define env (interaction-environment))

    (define (json:ref key)
         (cond
            ((string? key)
             (json:ref (string->symbol key)))
            ((number? key)
             (lambda (nodes) (vector-ref nodes key)))
            (else
                (lambda (node)
                     (cdr (assq key node))))))
     
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

     (define (json:write node)
         (with-output-to-string 
             (lambda () (json-write node))))

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
            (lambda (x) (eval x env))))

     (define (interpret-function-rule rule)
         (if (procedure? rule) rule
         (let ((func (car rule))
               (args (cdr rule)))
             (cond
                ((eq? func '*)
                 ;for-each: Execute for each value in vector.
                 ;Input is a vector of nodes instead of just a single node
                 (lambda (nodes) (vector-map (json:query args) nodes)))

                ((eq? func '*_)
                 ;for-each+flatten Execute for each value in vector, then flatten.
                 (lambda (nodes) 
                     (-> nodes
                         (interpret-function-rule `(* ,@args))
                         json:flatten)))
                ((eq? func 'filter)
                 (lambda (nodes) 
                     (vector-filter 
                          (lambda (node)
                            (eval (execute-procedures (car args)
                                                      node)
                                  env))
                          nodes)))
                (else
                 ; For replace
                 (lambda (node) ((apply (to-json-function func)
                                        (execute-procedures args node))
                                 node)))))))

     (define (interpret-rule rule)
        (cond
            ((or (string? rule)
                 (number? rule))
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
