(use gauche.test)

(test-start "naoyat.automata.production")

(use naoyat.automata.production)
(test-module 'naoyat.automata.production)

(define (write-object-to-string obj)
  (let1 out (open-output-string)
    (write-object obj out)
    (get-output-string out)))


(test-section "<production> A -> BC")
(let1 prod (make <production> :head 'A :body '(B C))
  (test* "slot 'head" 'A (head-of prod))
  (test* "slot 'body" '(B C) (body-of prod))
  (test* "A -> BC" "A -> BC" (write-object-to-string prod))
  )

(test-section "<production*> A -> B")
(let1 prod (make <production*> :head 'A :bodies '((B)))
  (test* "slot 'head" 'A (head-of prod))
  (test* "slot 'bodies" '((B)) (bodies-of prod))
  (test* "A -> B" "A -> B" (write-object-to-string prod))
  )

(test-section "<production*> A -> BC")
(let1 prod (make <production*> :head 'A :bodies '((B C)))
  (test* "slot 'head" 'A (head-of prod))
  (test* "slot 'bodies" '((B C)) (bodies-of prod))
  (test* "A -> BC" "A -> BC" (write-object-to-string prod))
  )

(test-section "<production*> A -> BC | D")
(let1 prod (make <production*> :head 'A :bodies '((B C) (D)))
  (test* "slot 'head" 'A (head-of prod))
  (test* "slot 'bodies" '((B C) (D)) (bodies-of prod))
  (test* "A -> BC | D" "A -> BC | D" (write-object-to-string prod))
  )

(test-section "<production*> (A -> BC | D) == (A -> BC | D)")
(let ([prod1 (make <production*> :head 'A :bodies '((B C) (D)))]
      [prod2 (make <production*> :head 'A :bodies '((B C) (D)))])
  (test* "eq?" #f (eq? prod1 prod2))
  (test* "eqv?" #f (eqv? prod1 prod2))
  (test* "equal?" #t (equal? prod1 prod2))
  )

(test-section "<production*> (A -> BC | D) != (A -> D | BC)")
(let ([prod1 (make <production*> :head 'A :bodies '((B C) (D)))]
      [prod2 (make <production*> :head 'A :bodies '((D) (B C)))])
  (test* "eq?" #f (eq? prod1 prod2))
  (test* "eqv?" #f (eqv? prod1 prod2))
  (test* "equal?" #f (equal? prod1 prod2))
  )


(test-section "epsilon?")
(test* "'e" #f (epsilon? 'e))
(test* "'eps" #t (epsilon? 'eps))
(test* "'epsilon" #t (epsilon? 'epsilon))
(test* "'ε" #t (epsilon? 'ε))
(test* "'ϵ" #t (epsilon? 'ϵ))
(test* "#\E" #f (epsilon? #\E))
(test* "#\e" #f (epsilon? #\e))
(test* "#\ε" #f (epsilon? #\ε))
(test* "#\ϵ" #f (epsilon? #\ϵ))

(test-section "terminal? - NOTE:このシステムではシンボルのaはnon-terminal,キャラクタのaはterminal")
(test* "(char) #\a" #t (terminal? #\a))
(test* "(symbol) 'a" #f (terminal? 'a))
(test* "(symbol) 'A" #f (terminal? 'A))
(test* "(epsilon) 'eps" #t (terminal? 'eps))

(test-section "non-terminal?")
(test* "(char) #\a" #f (non-terminal? #\a))
(test* "(symbol) 'a" #t (non-terminal? 'a))
(test* "(symbol) 'A" #t (non-terminal? 'A))
(test* "(epsilon) 'eps" #f (non-terminal? 'eps))

(test-end)
