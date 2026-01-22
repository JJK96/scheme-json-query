; Uncomment for static compilation
;(declare (unit util))
(module (util) (->
                tree-map
                tree-filter
                vector-filter
                vector-map
                indent-accumulator
                json-accumulator)
    (import scheme
            (only srfi-13 string-pad string-for-each)               
            (only srfi-1 filter fold make-list for-each))

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
          
    (define (write-unicode-escape ord)
      (display #\\)             
      (display #\u)
      (-> ord
          (lambda (c) (number->string c 16))    
          (lambda (s) (string-pad s 4 #\0))
          display))

    (define (write-json-string str)   
      (display #\")      
      (string-for-each
        (lambda (char)
          (let ((ord (char->integer char)))
            (if (member char '(#\\ #\"))
                (display #\\))
            (if (< ord 32)
                (write-unicode-escape ord)
                (display char))))
        str)
      (display #\"))

    (define (json-accumulator txt)
          (cond ((char? txt)
                 (display txt))
                ((string? txt)
                 (write-json-string txt))
                (else (write txt))))

    (define (indent-accumulator accumulator indent-step)
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
            (if (not (eq? last #\:))
                (begin
                  (newline)
                  (indent)))
            (push-scope txt)
            (accumulator txt)
            (newline))
          ((eq? txt #\:) (accumulator txt) (display " "))
          ((eq? txt #\,) (accumulator txt))
          ((eq? txt #\})
            (pop-scope)
            (newline)
            (indent)
            (accumulator txt))
          ((eq? txt #\[)
            (push-scope txt)
            (accumulator txt))
          ((eq? txt #\])
            (pop-scope)
            (newline)
            (indent)
            (accumulator txt))
          (else
            (if (member last '(#\, #\[))
                (newline))
            (indent)
            (accumulator txt)))
        (set! last txt)))
)
