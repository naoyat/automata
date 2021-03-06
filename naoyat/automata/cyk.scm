;;
;; Cocke-Younger-Kasami algorithm
;;
(define-module naoyat.automata.cyk
  (use srfi-1)
  (use gauche.array)
  (use util.combinations)
  (use naoyat.automata.cfg)
  (use naoyat.automata.production)

  (export make-CYK-solver))

(select-module naoyat.automata.cyk)

(define (make-CYK-solver cnf)
  (let1 rl (make-hash-table 'equal?)
    (dolist (prod (productions-of cnf))
      (let ([head (head-of prod)]
            [body (body-of prod)])
        (hash-table-put! rl body (cons head (hash-table-get rl body '())))))

    (lambda (w)
      (let* ([cs (list->vector (string->list w))]
             [len (vector-length cs)]
             [table (make-array (shape 0 len 0 len) '())])
        (dotimes (i len)
          (let1 c (vector-ref cs i)
            (let1 lh (hash-table-get rl (list c) '())
              (array-set! table i i lh))))

        (dolist (d (iota (- len 1) 1)) ; 幅-1
          (dotimes (i (- len d)) ;  w=2なら 0-1 1-2 2-3 3-4 なので 4; 4=len-(w-1) = len-(d+1)
            (let1 j (+ i d)
              (format #t "X~d~d:\n" i j)
              (dotimes (k d)
                (let ([s0 (array-ref table i (+ i k))]
                      [s1 (array-ref table (+ i k 1) j)])
                  (dolist (s01 (cartesian-product (list s0 s1)))
                    ;(print "s01: " s01)
                    (let1 lh (hash-table-get rl s01 #f)
                      (when lh
                        (format #t "  X~d~d + X~d~d = ~a -> ~a\n" i (+ i k) (+ i k 1) j s01 lh)
                        (let1 curr (array-ref table i j)
                          (array-set! table i j (lset-union eq? lh curr))))))))
              (format #t "  = ~a\n" (array-ref table i j)))))
        table))))
