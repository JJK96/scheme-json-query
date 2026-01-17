(library (debug)
     (export displayln
             debug)
     (import (rnrs))

     (define (displayln . x)
         (map
          (lambda (x)
             (display x)
             (display "\n"))
          x))

     (define (debug x)
         (displayln x)
         x)
)
