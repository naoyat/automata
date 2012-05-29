;; 与えられたリスト ls の要素の重複を排除して返す
(define (uniq ls . args)
  (let* ([hash-type (if (null? args) 'eq? (car args))]
         [ht (make-hash-table hash-type)])
    (for-each (cut hash-table-put! ht <> #t) ls)
    (hash-table-keys ht)))

;; 与えられたリスト ls の全要素を持つハッシュテーブルを作る
(define (list->set ls . args)
  (let* ([hash-type (if (null? args) 'eq? (car args))]
         [ht (make-hash-table hash-type)])
    (for-each (cut hash-table-put! ht <> #t) ls)
    ht))

;; ある要素が予め与えられたリストに含まれるか否かを返す関数を作成
(define (make-set-has?-proc ls . args)
  (let1 ht (apply list->set ls args)
    (lambda (elem) (hash-table-get ht elem #f))))

;; 与えられたリスト ls の全要素に連番を振ったハッシュテーブルを作る
(define (list->idmap ls . args)
  (let* ([hash-type (if (null? args) 'eq? (car args))]
         [ht (make-hash-table hash-type)])
    (let loop ((id 0) (ls ls))
      (if (null? ls) ht
          (begin
            (hash-table-put! ht (car ls) id)
            (loop (+ id 1) (cdr ls)))))))
