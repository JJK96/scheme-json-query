(library (util)
    (export ->
            vector-filter)
    (import (rnrs))

     (define (-> init . funcs)
         (fold-left 
                (lambda (val f) (f val))
                init 
                funcs))

     (define (vector-filter func v)
         (-> v
             vector->list
             (lambda (x) (filter func x))
             list->vector))

)
