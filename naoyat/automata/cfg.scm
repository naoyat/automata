(define-module naoyat.automata.cfg
  (use srfi-1)
  (use util.combinations)
  (use util.toposort)
  (use naoyat.automata.util)
  (use naoyat.automata.production)

  (export <CFG>
          start-symbol-of
          terminals-of
          non-terminals-of
          productions-of

          write-object
		  object-equal?

          make-CFG
          grammar->CFG

		  nullable-symbols

		  remove-epsilon-productions
		  remove-unit-productions
		  remove-useless-productions
		  remove-unreachable-productions
		  CFG-cleanup

		  <CNF>
		  CFG->CNF
		  make-CNF
          grammar->CNF
          ))
(select-module naoyat.automata.cfg)

(define-class <CFG> ()
  ((start-symbol  :init-keyword :start-symbol  :accessor start-symbol-of)
   (terminals     :init-keyword :terminals     :accessor terminals-of)
   (non-terminals :init-keyword :non-terminals :accessor non-terminals-of)
   (productions   :init-keyword :productions   :accessor productions-of)))

(define-method write-object ((cfg <CFG>) port)
  (map (cut format port "~a\n" <>) (productions-of cfg)))

(define-method object-equal? ((a <CFG>) (b <CFG>))
  (and (equal? (start-symbol-of a) (start-symbol-of b))
	   (equal? (sort (terminals-of a)) (sort (terminals-of b)))
	   (equal? (sort (non-terminals-of a) symbol<?) (sort (non-terminals-of b) symbol<?))
	   (productions-inpo-equal? (productions-of a) (productions-of b))))

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

(define (grammar->CFG g)
  (make-CFG (map (^(prod)
                   (make <production*>
                     :head (car prod)
                     :bodies (split-list '/ (cddr prod))))
                 g)))


(define (CFG-separated-productions cfg)
  (append-map (^(prod) (map (cute cons (head-of prod) <>) (bodies-of prod)))
			  (productions-of cfg)))

(define (nullable-symbols cfg)
  (let ([productions (make-hash-table 'equal?)]
		[nullables (make-hash-table 'eq?)])
	(for-each (cut hash-table-put! productions <> #t)
			  (CFG-separated-productions cfg))
	(let loop ((res '()))
	  (let1 curr '()
		(hash-table-for-each productions
							 (^(k v)
							   (let ([head (car k)]
									 [body (cdr k)])
								 (when (every (^(var)
												(or (epsilon? var)
													(hash-table-get nullables var #f)))
											  body)
								   (unless (hash-table-exists? nullables head)
									 (push! curr head)
									 (hash-table-put! nullables head #t))
								   (hash-table-delete! productions k)))))
		(if (null? curr) res
			(loop (append res (reverse! curr))))))))

(define (epsilon-body? body) (and (= 1 (length body)) (epsilon? (car body))))
		
(define (possible-subsets body nullables-ht)
  (define (sub prefix rest)
	(if (null? rest)
		(if (null? prefix) '()
			(list (reverse prefix)))
		(let1 a (car rest)
		  (if (hash-table-get nullables-ht a #f)
			  (append (sub (cons a prefix) (cdr rest))
					  (sub prefix (cdr rest)))
			  (sub (cons a prefix) (cdr rest))))))
  (if (epsilon-body? body) '()
	  (sub '() body)))

(define (ht->prods ht start-symbol)
  (cons (make <production*> :head start-symbol :bodies (hash-table-get ht start-symbol '()))
		(filter identity (hash-table-map ht
										 (^(head bodies)
										   (if (eq? head start-symbol) #f
											   (make <production*> :head head :bodies bodies)))))))

(define (remove-epsilon-productions cfg)
  (let1 nullables-ht (make-hash-table 'eq?)
	(for-each (cut hash-table-put! nullables-ht <> #t)
			  (nullable-symbols cfg))
	(let ([subsets-ht (make-hash-table 'eq?)]
		  [var-to-eliminate '()])
	  (dolist (prod (productions-of cfg))
		(let ([head (head-of prod)]
			  [body-subsets (append-map (cut possible-subsets <> nullables-ht)
										(bodies-of prod))])
		  (if (null? body-subsets)
			  (push! var-to-eliminate head)
			  (hash-table-put! subsets-ht head body-subsets))))
	  (let loop ((vars var-to-eliminate))
		(if (null? vars)
			(let1 new-prods (ht->prods subsets-ht (start-symbol-of cfg))
			  (make <CFG>
				:start-symbol (start-symbol-of cfg)
				:terminals (terminals-of cfg)
				:non-terminals (map head-of new-prods)
				:productions new-prods))
			(let1 var (car vars) ;; to eliminate
			  (let1 next-elim
				  (filter identity
						  (hash-table-map subsets-ht
										  (^(head body-subsets)
											(let1 el-subsets (remove (cut memq var <>) body-subsets)
											  (if (null? el-subsets)
												  (begin (hash-table-erase! subsets-ht head) head)
												  (begin (hash-table-put! subsets-ht head el-subsets) #f))))))
				(loop (append next-elim (cdr vars))))))))))

(define (remove-unit-productions cfg)
  (let ([pairs-ht (make-hash-table 'eq?)]
		[prods-ht (make-hash-table 'eq?)])
	(dolist (prod (productions-of cfg))
	  (let ([head (head-of prod)]
			[bodies (bodies-of prod)])
		(hash-table-put! prods-ht head bodies)
		(when (and (= 1 (length bodies))
				   (= 1 (length (car bodies)))
				   (non-terminal? (caar bodies)))
		  (hash-table-put! pairs-ht head (caar bodies)))))
	(if (zero? (hash-table-num-entries pairs-ht)) cfg
		(let* ([ht (make-hash-table 'eq?)]
			   [pairs (hash-table-map pairs-ht (^(head body) (list head body)))]
			   [sorted (reverse! (topological-sort pairs))])
		  (dolist (var sorted)
			(let1 next (hash-table-get pairs-ht var #f)
			  (when next
				(hash-table-put! prods-ht var (hash-table-get prods-ht next #f)))))
		  (let1 new-prods (ht->prods prods-ht (start-symbol-of cfg))
			(make <CFG>
			  :start-symbol (start-symbol-of cfg)
			  :terminals (terminals-of cfg)
			  :non-terminals (map head-of new-prods)
			  :productions new-prods))))))

(define (remove-useless-productions cfg)
  (let ([non-terminals (non-terminals-of cfg)]
		[productions (productions-of cfg)]
		[useful-ht (make-hash-table 'eq?)])
	(define (useful? var) (hash-table-get useful-ht var #f))
	(let loop ()
	  (let1 cnt 0
		(dolist (prod productions)
		  (let1 head (head-of prod)
			(unless (useful? head)
			  (when (any (^(body)
						   (every (^(var) (or (terminal? var) (useful? var)))
								  body))
						 (bodies-of prod))
				(inc! cnt)
				(hash-table-put! useful-ht head #t)))))
		(unless (zero? cnt) (loop))))
	(let1 new-prods (filter-map (^(prod)
								  (let ([head (head-of prod)]
										[bodies (bodies-of prod)])
									(if (useful? head)
										(let1 useful-bodies (filter (cut every (^(var) (or (terminal? var) (useful? var))) <>)
																	bodies)
										  (if (null? useful-bodies) #f
											  (make <production*> :head head :bodies useful-bodies)))
										#f)))
								productions)
	  (make-CFG new-prods))))

(define (remove-unreachable-productions cfg)
  (let ([prod-ht (make-hash-table 'eq?)]
		[reachables-ht (make-hash-table 'eq?)])
	(define (visited? var) (hash-table-get reachables-ht var #f))
	(dolist (prod (productions-of cfg))
	  (hash-table-put! prod-ht (head-of prod) (bodies-of prod)))
	(let loop ((var (start-symbol-of cfg)))
	  ;;(format #t "LOOP, var=~a/ ss=~a\n" var (start-symbol-of cfg))
	  (unless (visited? var)
		(hash-table-put! reachables-ht var #t)
;		(when (non-terminal? var)
		(dolist (body (hash-table-get prod-ht var '()))
		  (dolist (v body)
			(when (non-terminal? v) (loop v))))))
	;(unless (visited? v) (loop v)))))))
;	(print "visited: " (hash-table-keys reachables-ht))
	(let1 new-prods (filter (^(prod) (visited? (head-of prod))) (productions-of cfg))
;	(let1 new-prods (productions-of cfg)
	  (make-CFG new-prods))))

(define (CFG-cleanup cfg)
  (remove-unreachable-productions
   (remove-useless-productions
	(remove-unit-productions
	 (remove-epsilon-productions cfg)))))

(define-class <CNF> (<CFG>) ())

#;(define-method write-object ((cnf <CNF>) port)
  (map (cut format port "~a\n" <>) (productions-of cnf)))

(define-method object-equal? ((a <CNF>) (b <CNF>))
  (and (equal? (start-symbol-of a) (start-symbol-of b))
	   (equal? (sort (terminals-of a)) (sort (terminals-of b)))
	   (equal? (sort (non-terminals-of a) symbol<?) (sort (non-terminals-of b) symbol<?))
	   (equal? (productions-of a) (productions-of b))))

(define (make-CNF productions)
  (let ([NT (make-hash-table 'eq?)]
        [T (make-hash-table 'eq?)]
        [start-symbol #f])
    (dolist (prod productions)
      (let ([head (head-of prod)]
            [body (body-of prod)])
        (unless start-symbol (set! start-symbol head))
        (hash-table-put! NT head #t)
		(dolist (elem body)
		  (cond [(char? elem) (hash-table-put! T elem #t)]
				[(string? elem) (hash-table-put! T elem #t)]
				[else #f]))))
    (let ([non-terminals (hash-table-keys NT)]
          [terminals (hash-table-keys T)])
      (make <CNF>
        :start-symbol  start-symbol
        :terminals     terminals
        :non-terminals non-terminals
        :productions   productions))))

(define (grammar->CNF g)
  (make-CNF (map (^(prod)
                   (make <production>
                     :head (car prod)
                     :body (cddr prod)))
                 g)))

(define (CFG->CNF cfg)
  (define serial 0)
  (define (mygensym) (inc! serial) (string->symbol (format #f "G~d" serial)))
  (define (normalize head body)
	;;(format #t "normalize : ~a : ~a\n" head body)
	(let* ([trans '()]
		   [body* (map (^(var) (if (terminal? var)
								   (let1 newvar (mygensym)
									 (push! trans (make <production> :head newvar :body (list var)))
									 newvar)
								   var))
					   body)])
	  (append (if (= 2 (length body*))
				  (list (make <production> :head head :body body*))
				  (let1 newsym (mygensym)
					(cons (make <production> :head head :body (list (car body*) newsym))
						  (normalize newsym (cdr body*)))))
			  (reverse! trans))))
  (define (divide-production prod)
	(let ([head (car prod)]
		  [body (cdr prod)])
;;	  (format #t "div. ~a - ~a\n" head body)
	  (case (length body)
		[(1) (if (terminal? (car body))
				 (list (make <production> :head head :body body))
;				 (list (make <production*> :head head :bodies (list body)))
				 #f)]
		[(2) (if (and (non-terminal? (car body))
					  (non-terminal? (cadr body)))
				 (list (make <production> :head head :body body))
;				 (list (make <production*> :head head :bodies (list body)))
				 (normalize head body))]
		[else (normalize head body)])))
  (let* ([clean-cfg (CFG-cleanup cfg)]
		 [new-prods (filter identity (append-map divide-production (CFG-separated-productions clean-cfg)))])
	;new-prods
	(make <CNF>
	  :start-symbol (start-symbol-of clean-cfg)
	  :terminals (terminals-of clean-cfg)
	  :non-terminals (map head-of new-prods)
	  :productions new-prods)))

