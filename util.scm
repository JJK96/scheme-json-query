; Uncomment for static compilation
;(declare (unit util))
(module (util) (->
                tree-map
                tree-filter
                vector-filter
                vector-map
                indenting-accumulator)
    (import scheme
            (only srfi-1 filter fold make-list))

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

    (define (indenting-accumulator indent-step)
            (define (write-txt txt)
          (if (char? txt)
                  (display txt)
                  (write txt)))
      (define (newline)
        (display "\n"))
      (define scopes '())
      (define last #f)
      (define (push-scope char)
        (set! scopes (cons char scopes)))
      (define (pop-scope)
        (set! scopes (cdr scopes)))
      (define (indent)
        (if (not (eq? last #\:))
            (display (apply string-append (make-list (length scopes) indent-step)))))
      (lambda (txt)
        (cond
          ((eq? txt #\{)
            (if (not (member last '(#\[ #\,)))
                (indent))
            (push-scope txt)
            (write-txt txt)
            (newline))
          ((eq? txt #\:) (write-txt txt))
          ((eq? txt #\,) (write-txt txt))
          ((eq? txt #\})
            (pop-scope)
            (newline)
            (indent)
            (write-txt txt))
          ((eq? txt #\[)
            (push-scope txt)
            (write-txt txt))
          ((eq? txt #\])
            (pop-scope)
            (newline)
            (indent)
            (write-txt txt))
          (else
            (if (member last '(#\, #\[))
                (newline))
            (if (not (eq? last #\:))
                (indent))
            (write-txt txt)))
        (set! last txt)))
)
