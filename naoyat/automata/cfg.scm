(define-module naoyat.automata.cfg
  (use srfi-1)
  (use naoyat.automata.util)
  (use naoyat.automata.production)

  (export <CFG>
		  start-symbol-of
		  terminals-of
		  non-terminals-of
		  productions-of

		  make-cfg
		  grammar->CFG
		  ))
(select-module naoyat.automata.cfg)


#|
CFG-prouction の代わりに <production*> を
(define (make-CFG-production lhs rhs-list) (cons lhs rhs-list))
(define (CFG-production-lhs prod) (car prod))
(define (CFG-production-rhs-list prod) (cdr prod))
(define (CFG-production-pp prod)
  (define (rhs->string r)
    (string-join (map x->string r) ""))
  (format #t "~a -> " (CFG-production-lhs prod))
  (print-with-sep " | " (map rhs->string (CFG-production-rhs-list prod)))
  (newline))
|#

(define-class <CFG> ()
  ((start-symbol  :init-keyword :start-symbol  :accessor start-symbol-of)
   (terminals     :init-keyword :terminals     :accessor terminals-of)
   (non-terminals :init-keyword :non-terminals :accessor non-terminals-of)
   (productions   :init-keyword :productions   :accessor productions-of)))

#|
(define (make-CFG start terminals non-terminals productions)
  (list 'CFG start terminals non-terminals productions))
(define (CFG-start cfg) (second cfg))
(define (CFG-terminals cfg) (third cfg))
(define (CFG-non-terminals cfg) (fourth cfg))
(define (CFG-productions cfg) (fifth cfg))
|#

(define (make-CFG productions)
  (let ([NT (make-hash-table 'eq?)]
        [T (make-hash-table 'eq?)]
        [start-symbol #f])
    (dolist (prod productions)
      (let ([head (head-of prod)]
            [bodies (bodies-of prod)])
        (unless start-symbol (set! start-symbol head))
        (hash-table-put! NT head #t)
        (dolist (body bodies)
          (dolist (elem body)
            (cond [(char? elem) (hash-table-put! T elem #t)]
                  [(string? elem) (hash-table-put! T elem #t)]
                  [else #f])))))
    (let ([non-terminals (hash-table-keys NT)]
          [terminals (hash-table-keys T)])
      (make <CFG>
		:start-symbol  start-symbol
		:terminals     terminals
		:non-terminals non-terminals
		:productions   productions))))

#;(define (CFG-pp cfg)
  (print "start-symbol: " (start-symbol-of cfg))
  (print "terminals: " (terminals-of cfg))
  (print "non-terminals: " (non-terminals-of cfg))
  (print "productions: ") (map print (productions-of cfg)))

;(define (CNF-production-lhs cprod) (car cprod))
;(define (CNF-production-rhs-list cprod) (cddr cprod))

;; Note: CNFじゃなくても通るのでCNFというのは適切じゃない。CNFはCNFで専用の型を考えたい
(define (grammar->CFG cnf)
  (make-CFG (map (lambda (prod)
				   (make <production*>
					 :head (car prod)
					 :bodies (split-list '/ (cddr prod))
					 ))
				 cnf)))
