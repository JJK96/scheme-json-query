; Uncomment for static compilation
;(declare (unit util))
(module (util) (->
                tree-map
                tree-filter
                vector-filter
                vector-map)
    (import scheme
            (only srfi-1 filter fold))

    (define (-> init . funcs)
        (fold 
            (lambda (f val) (f val))
            init 
            funcs))

    (define (ho-vector ho func v)
        (-> v
             vector->list
             (lambda (x) (ho func x))
             list->vector))

    (define (vector-filter func v)
         (ho-vector filter func v))

    (define (vector-map func v)
        (ho-vector map func v))

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

    (define (tree-filter func)
      (lambda (tree)
        (cond
          ((null? tree)
           '())
          ((vector? tree)
           (vector-filter func (vector-map (tree-filter func) tree)))
          ((list? tree)
           (filter func (map (tree-filter func) tree)))
          (else tree))))

)
