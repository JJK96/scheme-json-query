; Uncomment for static compilation
;(declare (unit util))
(module (util) (->
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
)
