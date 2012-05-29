(require "./CFG.scm")

(use gauche.array)
(use util.combinations)

;; とりあえずrhsが2アイテムまでの文法のみ
(define (make-CYK-solver cfg)
  (let1 rl (make-hash-table 'equal?)

    (dolist (prod (CFG-productions cfg))
      (let ([lhs (CFG-production-lhs prod)]
            [rhsl (CFG-production-rhs-list prod)])
        (dolist (rhs rhsl)
          (hash-table-put! rl rhs
                           (cons lhs (hash-table-get rl rhs '()))))))

;   (hash-table-for-each rl (lambda (k v) (format #t "{ ~a ~a }\n" k v)))

    (lambda (w)
      (let* ([cs (list->vector (string->list w))]
             [len (vector-length cs)]
             [table (make-array (shape 0 len 0 len) '())])

        (dotimes (i len)
          (let1 c (vector-ref cs i)
            (let1 lh (hash-table-get rl (list c) '())
              (array-set! table i i lh))))

        (dolist (d (iota (- len 1) 1)) ; 幅-1
          ;(format #t "[width=~d]\n" (+ d 2))
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
                          (array-set! table i j (lset-union eq? lh curr))
                          ))))))
              (format #t "  = ~a\n" (array-ref table i j))
              )))

        ;(hash-table-for-each id-ht (lambda (k v) (print k " " v)))
        ;(print table)
        ;(format #t "X_0~d = ~d\n" (- len 1) (array-ref table 0 (- len 1)))
        table))))
