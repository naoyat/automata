(define-module naoyat.automata.production
  (export <production>
          head-of
          body-of

          <production*>
          bodies-of

          write-object
          object-equal?

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


;;
;; terminal / non-terminal
;;
(define epsilon 'ϵ)
(define (epsilon? var) (and (memq var '(eps epsilon ε ϵ)) #t))
(define (terminal? var) (or (char? var) (epsilon? var)))
(define (non-terminal? var) (and (symbol? var) (not (epsilon? var))))
