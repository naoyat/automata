;;
;; Regular Language
;;
(define-module naoyat.automata.regular
  (use srfi-1) ;; cons* -> Gauche の組み込み list* でも可
  (use gauche.array)
  (use naoyat.automata.util)
  (use naoyat.automata.fa)

  (export *ullman*

          Single Concat Union
          Kleene* Kleene+ Kleene?
          char-range

          regexp->NFA
          string->NFA
          ))
(select-module naoyat.automata.regular)


(define *ullman* #f) ;; Ullman先生方式なら#t

;; single input
(define (Single input)
  (let ([start (genstate)]
        [final (genstate)])
    (make <FA>
      :start-state  start
      :final-states (list final)
      :states       (list start final)
      :transitions  (list (make <FA-transition>
                            :state1 start
                            :input  input
                            :state2 final)))))

;; concatenation (AB..)
(define (Concat . faa)
  (let* ([fa1 (car faa)]
         [fa1-start (start-state-of fa1)])
    (let loop ((last-finals (final-states-of fa1))
               (states (states-of fa1))
               (transitions (transitions-of fa1))
               (rest (cdr faa)))
      (if (null? rest)
          (make <FA>
            :start-state fa1-start
            :final-states last-finals
            :states states
            :transitions transitions)
          (let* ([fa2 (car rest)]
                 [concat-trs (map (cut make <FA-transition>
                                       :state1 <>
                                       :input 'eps
                                       :state2 (start-state-of fa2))
                                  last-finals)])
            (loop (final-states-of fa2)
                  (append states (states-of fa2))
                  (append transitions concat-trs (transitions-of fa2))
                  (cdr rest)))))))

;; union (A + B + ..)
(define (Union . faa)
  (let ([start (genstate)]
        [final (genstate)])
    (let ([states+
           (append (list start) (append-map states-of faa) (list final))]
          [transitions+
           (append-map (lambda (fa)
                         (let ([s-> (make <FA-transition>
                                      :state1 start
                                      :input 'eps
                                      :state2 (start-state-of fa))]
                               [->f* (map (cut make <FA-transition>
                                               :state1 <>
                                               :input 'eps
                                               :state2 final)
                                          (final-states-of fa))])
                           (append (list s->) (transitions-of fa) ->f*)))
                       faa)])
      (make <FA>
        :start-state start
        :final-states (list final)
        :states states+
        :transitions transitions+))))

(define (char-range from-char to-char)
  (let ([from (char->integer from-char)]
        [to (char->integer to-char)])
    (reverse! (map integer->char (iota (+ (- to from) 1) from)))))

;; Kleene star (A*)
(define (Kleene* a)
  (let ([start (genstate)]
        [final (genstate)]
        [a-start (start-state-of a)]
        [a-finals (final-states-of a)]
        [states (states-of a)]
        [transitions (transitions-of a)])
    (let ([start->final (make <FA-transition>
                          :state1 start
                          :input 'eps
                          :state2 final)]
          [start->a-start (make <FA-transition>
                            :state1 start
                            :input 'eps
                            :state2 a-start)]
          [a-finals->final (map (cut make <FA-transition>
                                     :state1 <>
                                     :input 'eps
                                     :state2 final)
                                a-finals)]
          [a-finals->a-start (map (cut make <FA-transition>
                                       :state1 <>
                                       :input 'eps
                                       :state2 (if *ullman* a-start start))
                                  a-finals)])
      (let ([states+ (cons* start final states)]
            [transitions+ (append (list start->final start->a-start)
                                  a-finals->final a-finals->a-start transitions)])
        (make <FA>
          :start-state start
          :final-states (list final)
          :states states+
          :transitions transitions+)))))

;; Kleene plus (A+)
(define (Kleene+ a)
  (let ([start (genstate)]
        [final (genstate)]
        [a-start (start-state-of a)]
        [a-finals (final-states-of a)]
        [states (states-of a)]
        [transitions (transitions-of a)])
    (let ([start->a-start (make <FA-transition>
                            :state1 start
                            :input 'eps
                            :state2 a-start)]
          [a-finals->final (map (cut make <FA-transition>
                                     :state1 <>
                                     :input 'eps
                                     :state2 final)
                                a-finals)]
          [a-finals->a-start (map (cut make <FA-transition>
                                       :state1 <>
                                       :input 'eps
                                       :state2 (if *ullman* a-start start))
                                  a-finals)])
      (let ([states+ (cons* start final states)]
            [transitions+ (append (list start->a-start)
                                  a-finals->final a-finals->a-start transitions)])
        (make <FA>
          :start-state start
          :final-states (list final)
          :states states+
          :transitions transitions+)))))

;; A?
(define (Kleene? a)
  (let ([start (genstate)]
        [final (genstate)]
        [a-start (start-state-of a)]
        [a-finals (final-states-of a)]
        [states (states-of a)]
        [transitions (transitions-of a)])
    (let ([start->final (make <FA-transition>
                          :state1 start
                          :input 'eps
                          :state2 final)]
          [start->a-start (make <FA-transition>
                            :state1 start
                            :input 'eps
                            :state2 a-start)]
          [a-finals->final (map (cut make <FA-transition>
                                     :state1 <>
                                     :input 'eps
                                     :state2 final)
                                a-finals)])
      (let ([states+ (cons* start final states)]
            [transitions+ (append (list start->final start->a-start)
                                  a-finals->final transitions)])
        (make <FA>
          :start-state start
          :final-states (list final)
          :states states+
          :transitions transitions+)))))



(define (regexp->NFA str)
  (define (read-bracket cs)
    (let loop ((cs cs) (chars '()))
      (let1 c (car cs)
        (cond [(eq? #\] c)
               (values (apply Union (map Single (reverse! chars))) (cdr cs))]
              [(eq? #\- c)
               (if (null? chars)
                   (loop (cdr cs) (cons #\- chars))
                   (loop (cddr cs) (append (char-range (car chars) (cadr cs))
                                           (cdr chars))))]
              [else
               (loop (cdr cs) (cons c chars))]))))
  (let loop ((cs (string->list str)) (concat '()) (union '()))
    (define (concatted)
      (apply Concat (reverse! concat)))
    (define (render)
      (let1 u (reverse! (cons (concatted) union))
        (if (= 1 (length u))
            (car u)
            (apply Union u))))
    (if (null? cs) (render)
        (let1 c (car cs)
          (cond [(eq? #\( c)
                 (receive (obj rest) (loop (cdr cs) '() '())
                   (loop rest (cons obj concat) union))]
                [(eq? #\) c)
                 (values (render) (cdr cs))]
                [(eq? #\[ c)
                 (receive (obj rest) (read-bracket (cdr cs))
                   (loop rest (cons obj concat) union))]
                [(eq? #\* c)
                 (loop (cdr cs) (list (Kleene* (concatted))) union)]
                [(eq? #\+ c)
                 (loop (cdr cs) (list (Kleene+ (concatted))) union)]
                [(eq? #\? c)
                 (loop (cdr cs) (list (Kleene? (concatted))) union)]
                [(eq? #\| c)
                 (loop (cdr cs) '() (cons (concatted) union))]
                [else
                 (loop (cdr cs) (cons (Single (car cs)) concat) union)] )))))

(define (string->NFA str)
  (let loop ((cs (string->list str)) (concat '()) (union '()))
    (define (concatted)
      (apply Concat (reverse! concat)))
    (define (render)
      (let1 u (reverse! (cons (concatted) union))
        (if (= 1 (length u))
            (car u)
            (apply Union u))))
    (if (null? cs) (render)
        (let1 c (car cs)
          (cond [(eq? #\( c)
                 (receive (obj rest) (loop (cdr cs) '() '())
                   (loop rest (cons obj concat) union))]
                [(eq? #\) c)
                 (values (render) (cdr cs))]
                [(eq? #\* c)
                 (loop (cdr cs) (list (Kleene* (concatted))) union)]
                [(eq? #\+ c)
                 (loop (cdr cs) '() (cons (concatted) union))]
                [else
                 (loop (cdr cs) (cons (Single (car cs)) concat) union)] )))))
