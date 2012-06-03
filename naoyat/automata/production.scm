(define-module naoyat.automata.production
  (use naoyat.automata.util)

  (export <production>
          head-of
          body-of

          <production*>
          bodies-of

          write-object
          object-equal?

          productions-inpo-equal?

          epsilon
          epsilon?
          terminal?
          non-terminal?))

(select-module naoyat.automata.production)

;;
;; production
;;
;;   A -> B C
;;
;;    head : 'A
;;    body : '(B C)
;;
(define-class <production> ()
  ((head :init-keyword :head :accessor head-of)
   (body :init-keyword :body :accessor body-of)))

(define (display-single-body rhs port)
  (dolist (var rhs) (display var port)))

(define-method write-object ((prod <production>) port)
  (format port "~a -> " (head-of prod))
  (display-single-body (body-of prod) port))

(define-method object-equal? ((a <production>) (b <production>))
  (and (equal? (head-of a) (head-of b))
       (equal? (body-of b) (body-of b))))

;;
;; production*
;;
;;   A -> B C | D E | f
;;
;;    head : 'A
;;    body : '((B C) (D E) (f))
;;
(define-class <production*> ()
  ((head   :init-keyword :head   :accessor head-of)
   (bodies :init-keyword :bodies :accessor bodies-of)))

(define-method write-object ((prod <production*>) port)
  (format port "~a -> " (head-of prod))
  (let1 bodies (bodies-of prod)
    (display-single-body (car bodies) port)
    (dolist (var (cdr bodies)) (display " | " port) (display-single-body var port))))

(define-method object-equal? ((a <production*>) (b <production*>))
  (and (equal? (head-of a) (head-of b))
       (equal? (bodies-of a) (bodies-of b))))

(define (var<? v1 v2)
  (cond [(char? v1)
		 (if (char? v2) (char<? v1 v2) #t)]
		[(symbol? v1)
		 (if (symbol? v2) (symbol<? v1 v2) #f)]
		[else #f]))

(define (productions-inpo-equal? p1 p2)
  (let ([p1-ht (make-hash-table 'eq?)]
		[p2-ht (make-hash-table 'eq?)])
	(dolist (prod p1) (hash-table-put! p1-ht (head-of prod) (bodies-of prod)))
	(dolist (prod p2) (hash-table-put! p2-ht (head-of prod) (bodies-of prod)))
	(let ([nt1 (sort (hash-table-keys p1-ht) symbol<?)]
		  [nt2 (sort (hash-table-keys p2-ht) symbol<?)])
	  (and (equal? nt1 nt2)
		   (every (^(head)
					(equal? (sort (hash-table-get p1-ht head '()) var<?)
							(sort (hash-table-get p2-ht head '()) var<?)))
				  (hash-table-keys p1-ht))))))

;;
;; terminal / non-terminal
;;
(define epsilon 'ϵ)
(define (epsilon? var) (and (memq var '(eps epsilon ε ϵ)) #t))
(define (terminal? var) (or (char? var) (epsilon? var)))
(define (non-terminal? var) (and (symbol? var) (not (epsilon? var))))
