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
                      json:filter
                      json:write
                      json:edit
                      set-result)
     (import scheme
             (chicken port)
             (chicken base)
             (chicken eval)
             (chicken module)
             (only srfi-180 json-write)
             (only vector-lib vector-map vector-append vector-copy)
             (only srfi-1 delete-duplicates filter)
             util)

    (define env (interaction-environment))
    (define set-result (make-parameter '()))
    (define json:querying (make-parameter #t))

    (define (json:traverse:ref key node next-rules)
        ; The idea here is to traverse into the value indicated by key
        ; and return the resulting value in-place
        (cond
            ((list? node)
             (map (lambda (entry)
                     (let ((k (car entry))
                           (v (cdr entry))
                           (key (if (string? key) 
                                    (string->symbol key) 
                                    key)))
                     (if (eq? key k)
                         (cons k (json:traverse* v next-rules))
                         entry)))
                node))
            ((vector? node)
             (let ((new-value (json:traverse* (vector-ref node key) next-rules))
                   (new-vector (vector-copy node)))
                (vector-set! new-vector key new-value)
                new-vector))))

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

     (define (json:filter func)
        (lambda (nodes)
            ; List of nodes or list of key-value entries
            (cond
                ((vector? nodes)
                 (vector-filter func nodes))
                ((list? nodes)
                 (filter func nodes))
                (else (error 'json:filter "Can only filter vector or node" nodes)))
        ))

     (define (json:unique nodes)
         (-> nodes
             vector->list
             delete-duplicates
             list->vector))

     (define (json:replace new-node)
         (lambda (node) new-node))

     (define (json:write node)
         (with-output-to-string 
             (lambda () (json-write node (indenting-accumulator "  ")))))

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

     (define (json:traverse:function node rule next-rules)
         (let ((func (car rule))
               (args (cdr rule)))
             (cond
                ((eq? func '*)
                 ;for-each: Execute for each value in vector.
                 ;Input is a vector of nodes instead of just a single node
                 (json:traverse*
                    (vector-map
                        (lambda (n) (json:traverse n args))
                        node)
                    next-rules))
                ((eq? func '*_)
                 ;for-each+flatten Execute for each value in vector, then flatten.
                 (json:traverse:function
                    node
                    `(* ,@args)
                    (cons json:flatten next-rules)))
                ((eq? func 'filter)
                 ; We make this a separate branch because we need to evaluate the first argument 
                 ; in order to execute it as a function
                 (json:traverse* 
                      ((json:filter
                          (lambda (n)
                            ; First execute all procedures in the arguments on the given node (e.g. (json:query ...))
                            (let ((condition-syntax (execute-procedures (car args) n)))
                             (eval condition-syntax env))))
                          node)
                      next-rules))
                (else (let ((func (apply (to-json-function func)
                                         (execute-procedures args node))))
                        (json:traverse* (func node) next-rules))))))

     (define (json:traverse* node rules)
        (if (null? rules) 
            ; If we are in json:query context overwrite the root object with the current node
            ; Otherwise (in json:edit context) return the current node in-place
            (if (json:querying)
                ((set-result) node)
                node)
        (let ((rule (car rules))
              (next-rules (cdr rules)))
          (cond
            ((or (string? rule)
                 (number? rule))
             (json:traverse:ref rule node (cdr rules)))
            ((procedure? rule)
             (json:traverse* (rule node) next-rules))
            ; e.g. keys, values, write, unique
            ((symbol? rule)
             (json:traverse* ((to-json-function rule) node) 
                            next-rules))
            ; e.g. filter, *, *_
            ((list? rule)
             (json:traverse:function node rule next-rules))
            (else (error 'json:traverse* "Incorrect rule" rule))))))

     (define (json:traverse node rules)
        (call/cc
            (lambda (cc)
                ; We set the parameter set-result to the current continuation
                ; This allows any function to immediately overwritet he root node by calling this continuation
                (parameterize ((set-result cc))
                    (json:traverse* node rules)))))

     (define (json:edit rules)
        (lambda (node)
            (parameterize ((json:querying #f))
                (json:traverse node rules))))
         
     (define (json:query rules)
        (lambda (node)
            (parameterize ((json:querying #t))
                (json:traverse node rules))))
         
)
