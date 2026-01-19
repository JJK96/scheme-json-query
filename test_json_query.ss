(import (json-query)
        (util))

(define-syntax test
    (syntax-rules ()
        ((_ name input desired-result)
         (guard (ex 
            (else (begin 
               (format #t "Exception while executing test ~a" name)
               (raise ex))))
            (let ((result input))
                (unless (equal? result desired-result)
                    (error name (format #f "\nExecuted:\n~a\nresult        : ~a\ndesired-result: ~a" 'input result desired-result))))))))

(define data '((a . (( b . "res" )))
               (b . #((( key . 1)) ((key . 2)) ((key . 3)) ((key . 4))))
               (c . #((( d . #(1 2 3 4))) (( d . #(5 6 7 8)))))
               (d . #((( key1 . 1) (key2 . 1)) (( key1 . 1) (key2 . 2))))))

(test 'single-ref
      ((json:query '("a"))
       data)
      '((b . "res")))

(test 'multiple-ref
      ((json:query '("a" "b"))
       data)
      "res")

(test 'procedure
      ((json:query `("a" ,json:keys))
       data)
      '#(b))

(test 'procedure-remapped
      ((json:query '("a" keys))
       data)
      '#(b))

(test 'procedure-remapped2
      ((json:query '("a" values))
       data)
      '#("res"))

(test 'node-list
      ((json:query '("b" (* "key")))
       data)
      '#(1 2 3 4))

(test 'filter-explicit
      ((json:query `("b" ,(lambda (nodes) (vector-filter (lambda (node) (eq? ((json:ref 'key) node) 2))
                                                         nodes))))
       data)
      '#(((key . 2))))

(test 'filter-macro
      ((json:query `("b" (filter ,(lambda (node) (eq? ((json:ref 'key) node)
                                                     2)))))
       data)
      '#(((key . 2))))

(test 'filter-macro-query
      ((json:query `("b" (filter (eq? ,(json:query '("key")) 2))))
       data)
      '#(((key . 2))))

(test 'filter-macro-inverted
      ((json:query `("b" (filter (eq? 2 ,(json:query '("key"))))))
       data)
      '#(((key . 2))))

(test 'not-equal
      ((json:query `("b" (filter (not (eq? 2 ,(json:query '("key")))))))
       data)
      '#(((key . 1)) ((key . 3)) ((key . 4))))

(test 'equal-values-different-keys
      ((json:query `("d" (filter (eq? ,(json:query '("key1"))
                                      ,(json:query '("key2"))))))
       data)
      '#(((key1 . 1) (key2 . 1))))

(test 'flatten
      ((json:query `("c" (* "d") flatten))
       data)
      '#(1 2 3 4 5 6 7 8))

(test 'for-each+flatten
      ((json:query `("c" (*_ "d")))
       data)
      '#(1 2 3 4 5 6 7 8))

(test 'unique
      ((json:query '("c" (* keys) flatten unique))
       data)
      '#(d))

(test 'replace
      ((json:query '("a" (replace ((c . "res")))))
       data)
      '((c . "res")))

(test 'replace-macro
      ((json:query `("a" (replace ((c . ,(json:query '("b")))))))
       data)
      '((c . "res")))

(test 'replace-macro1
      ((json:query `("d" (replace ((keys1 . ,(json:query '((* "key1"))))
                                   (keys2 . ,(json:query '((* "key2"))))))))
       data)
      '((keys1 . #(1 1)) (keys2 . #(1 2))))

(test 'after-replace
      ((json:query `("d" (replace ((keys1 . ,(json:query '((* "key1"))))
                                   (keys2 . ,(json:query '((* "key2"))))))
                         "keys1"))
       data)
      '#(1 1))

(test 'vector-ref
      ((json:query '("b" 1))
       data)
      '((key . 2)))

(test 'vector-ref-multiple
      ((json:query `("b" (replace #(,(json:query '(0))
                                    ,(json:query '(1))))))
       data)
      '#(((key . 1)) ((key . 2))))
