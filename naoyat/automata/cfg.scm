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

(define-class <CFG> ()
  ((start-symbol  :init-keyword :start-symbol  :accessor start-symbol-of)
   (terminals     :init-keyword :terminals     :accessor terminals-of)
   (non-terminals :init-keyword :non-terminals :accessor non-terminals-of)
   (productions   :init-keyword :productions   :accessor productions-of)))

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

(define (grammar->CFG cnf)
  (make-CFG (map (lambda (prod)
                   (make <production*>
                     :head (car prod)
                     :bodies (split-list '/ (cddr prod))))
                 cnf)))
