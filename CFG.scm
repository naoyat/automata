(use srfi-1)
(require "./myutil")

(define (make-CFG-production lhs rhs-list) (cons lhs rhs-list))
(define (CFG-production-lhs prod) (car prod))
(define (CFG-production-rhs-list prod) (cdr prod))
(define (CFG-production-pp prod)
  (define (rhs->string r)
    (string-join (map x->string r) ""))
  (format #t "~a -> " (CFG-production-lhs prod))
  (print-with-sep " | " (map rhs->string (CFG-production-rhs-list prod)))
  (newline))

(define (make-CFG start terminals non-terminals productions)
  (list 'CFG start terminals non-terminals productions))
(define (CFG-start cfg) (second cfg))
(define (CFG-terminals cfg) (third cfg))
(define (CFG-non-terminals cfg) (fourth cfg))
(define (CFG-productions cfg) (fifth cfg))

(define (make-CFG-from-productions productions)
  (let ([NT (make-hash-table 'eq?)]
        [T (make-hash-table 'eq?)]
        [start #f])
    (dolist (prod productions)
      (let ([lhs (CFG-production-lhs prod)]
            [rhs (CFG-production-rhs-list prod)])
        (unless start (set! start lhs))
        (hash-table-put! NT lhs #t)
        (dolist (r rhs)
          (dolist (elem r)
            (cond [(char? elem) (hash-table-put! T elem #t)]
                  [(string? elem) (hash-table-put! T elem #t)]
                  [else #f])))))
    (let ([non-terminals (hash-table-keys NT)]
          [terminals (hash-table-keys T)])
      (make-CFG start terminals non-terminals productions))))

(define (CFG-pp cfg)
  (print "start: " (CFG-start cfg))
  (print "terminals: " (CFG-terminals cfg))
  (print "non-terminals: " (CFG-non-terminals cfg))
  (print "productions: ")
  (map CFG-production-pp (CFG-productions cfg)))

(define (CNF-production-lhs cprod) (car cprod))
(define (CNF-production-rhs-list cprod) (cddr cprod))

;; Note: CNFじゃなくても通るのでCNFというのは適切じゃない。CNFはCNFで専用の型を考えたい
(define (CNF->CFG cnf)
  (define (CNF-production->production prod)
    (let ([lhs (car prod)]
          [rhs (split-list '/ (cddr prod))])
      (make-CFG-production lhs rhs)))
  (let1 prods (map CNF-production->production cnf)
    (make-CFG-from-productions prods)))

