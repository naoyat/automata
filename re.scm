(use srfi-1) ;; cons* -> Gauche の組み込み list* でも可

(define (uniq ls . args)
  (let* ([ht-type (if (null? args) 'eq? (car args))]
         [ht (make-hash-table ht-type)])
    (for-each (cut hash-table-put! ht <> #t) ls)
    (hash-table-keys ht)))

(define *ullman* #f) ;; Ullman先生方式なら#t
(define *verbose* #f)

;;; Finite Automata
(define (FA start finals states transitions)
  (list 'FA start finals states transitions))
(define (FA? f) (eq? 'FA (car f)))
(define (fa-start-state f) (second f))
(define (fa-final-states f) (third f))
(define (fa-states f) (fourth f))
(define (fa-transitions f) (fifth f))
(define (fa-prettyprint fa name)
  (format #t "Finite Automaton ~a:\n" name)
  (format #t "  states: ~a\n" (fa-states fa))
  (format #t "    start state: ~a\n" (fa-start-state fa))
  (format #t "    final states: ~a\n" (fa-final-states fa))
  (format #t "  transitions:\n")
  (for-each (lambda (tr)
              (format #t "    ")
              (trans-prettyprint tr))
            (fa-transitions fa)))

(define (fa-inputs fa)
  (uniq (map trans-input (fa-transitions fa))))

;; FA内のstateに番号を振る
(define (fa-renum fa)
  (let ([state-id 0]
        [state-id-ht (make-hash-table 'eq?)])

    (define (state->id state)
      (or (hash-table-get state-id-ht state #f)
          (let1 curr-id state-id
            (inc! state-id)
            (hash-table-put! state-id-ht state curr-id)
            curr-id)))

    (state->id (fa-start-state fa)) ; start state を 0 にするための呼び出し

    (FA (state->id (fa-start-state fa))
        (map state->id (fa-final-states fa))
        (map state->id (fa-states fa))
        (map (lambda (tr)
               (Trans (state->id (trans-state1 tr))
                      (trans-input tr)
                      (state->id (trans-state2 tr))))
             (fa-transitions fa)))))

;; FAのグラフをdot形式に変換
(define (fa->dot fa name)
  (format #t "digraph \"~a\" {\n" name)
  (format #t "  graph [ rankdir = LR ];\n")
  (format #t "  node [ fontname = \"Courier\", fontsize = 14, shape = circle, width = 0.3, height = 0.3, margin = 0.01 ];\n")
  (format #t "  edge [ fontname = \"Courier\", color = black, weight = 1 ];\n")
  (format #t "  start [ shape = plaintext ];\n")
  (format #t "  start -> ~a;\n" (fa-start-state fa))
  (dolist (st (fa-final-states fa))
    (format #t "  ~a [ shape = circle, peripheries = 2 ];\n" st))
  (dolist (tr (fa-transitions fa))
    (let1 input (trans-input tr)
      (when (eq? 'eps input) (set! input "ϵ"))
      (format #t "  ~a -> ~a [ label = \"~a\" ];\n" (trans-state1 tr) (trans-state2 tr) input)))
  (format #t "}\n"))

;; FAのグラフをGraphvizで画像化
(define (fa-draw fa label imgfile)
  (let* ([match (rxmatch #/([^.]+)\.(gif|png|ps|svg)/ imgfile)]
         [dotfile (format "~a.dot" (match 1))]
         [suffix (match 2)])
    (with-output-to-file dotfile
      (lambda () (fa->dot fa label)))
    (sys-system (format "dot -T~a ~a -o ~a\n" suffix dotfile imgfile))
    (sys-system (format "open ~a" imgfile))))


;;; States
(define (genstate) (gensym)) ;; uniqueなステート番号を振ってくれる何か

;;; Transitions
(define (Trans state1 input state2)
  (cons state1 (cons input state2)))
(define (trans-state1 tr) (car tr))
(define (trans-input tr) (cadr tr))
(define (trans-state2 tr) (cddr tr))
(define (trans-prettyprint tr)
  (let ([state1 (trans-state1 tr)]
        [state2 (trans-state2 tr)]
        [input (trans-input tr)])
  (if (eq? 'eps input)
      (format #t "~a + ϵ -> ~a\n" state1 state2)
      (format #t "~a + '~a' -> ~a\n" state1 input state2))))

;; single input
(define (Single input)
  (let ([start (genstate)]
        [final (genstate)])
    (let1 tr (Trans start input final)
      (FA start (list final) (list start final) (list tr)))))

;; concatenation (AB..)
(define (Concat . faa)
  (let* ([fa1 (car faa)]
         [fa1-start (fa-start-state fa1)])
    (let loop ((last-finals (fa-final-states fa1))
               (states (fa-states fa1))
               (transitions (fa-transitions fa1))
               (rest (cdr faa)))
      (if (null? rest)
          (FA fa1-start last-finals states transitions)
          (let* ([fa2 (car rest)]
                 [concat-trs (map (cut Trans <> 'eps (fa-start-state fa2)) last-finals)])
            (loop (fa-final-states fa2)
                  (append states (fa-states fa2))
                  (append transitions concat-trs (fa-transitions fa2))
                  (cdr rest)))))))

;; union (A + B + ..)
(define (Union . faa)
  (let ([start (genstate)]
        [final (genstate)])
    (let ([states+
           (append (list start) (append-map fa-states faa) (list final))]
          [transitions+
           (append-map (lambda (fa)
                         (let ([s-> (Trans start 'eps (fa-start-state fa))]
                               [->f* (map (cut Trans <> 'eps final) (fa-final-states fa))])
                           (append (list s->) (fa-transitions fa) ->f*)))
                       faa)])
      (FA start (list final) states+ transitions+))))

(define (char-range from-char to-char)
  (let ([from (char->integer from-char)]
        [to (char->integer to-char)])
    (reverse! (map integer->char (iota (+ (- to from) 1) from)))))

;; Kleene star (A*)
(define (Kleene* a)
  (let ([start (genstate)]
        [final (genstate)]
        [a-start (fa-start-state a)]
        [a-finals (fa-final-states a)]
        [states (fa-states a)]
        [transitions (fa-transitions a)])
    (let ([start->final (Trans start 'eps final)]
          [start->a-start (Trans start 'eps a-start)]
          [a-finals->final (map (cut Trans <> 'eps final) a-finals)]
          [a-finals->a-start (map (cut Trans <> 'eps (if *ullman* a-start start)) a-finals)])
      (let ([states+ (cons* start final states)]
            [transitions+ (append (list start->final start->a-start)
                                  a-finals->final a-finals->a-start transitions)])
        (FA start (list final) states+ transitions+)))))

;; Kleene plus (A+)
(define (Kleene+ a)
  (let ([start (genstate)]
        [final (genstate)]
        [a-start (fa-start-state a)]
        [a-finals (fa-final-states a)]
        [states (fa-states a)]
        [transitions (fa-transitions a)])
    (let ([start->a-start (Trans start 'eps a-start)]
          [a-finals->final (map (cut Trans <> 'eps final) a-finals)]
          [a-finals->a-start (map (cut Trans <> 'eps (if *ullman* a-start start)) a-finals)])
      (let ([states+ (cons* start final states)]
            [transitions+ (append (list start->a-start)
                                  a-finals->final a-finals->a-start transitions)])
        (FA start (list final) states+ transitions+)))))

;; A?
(define (Kleene? a)
  (let ([start (genstate)]
        [final (genstate)]
        [a-start (fa-start-state a)]
        [a-finals (fa-final-states a)]
        [states (fa-states a)]
        [transitions (fa-transitions a)])
    (let ([start->final (Trans start 'eps final)]
          [start->a-start (Trans start 'eps a-start)]
          [a-finals->final (map (cut Trans <> 'eps final) a-finals)])
      (let ([states+ (cons* start final states)]
            [transitions+ (append (list start->final start->a-start)
                                  a-finals->final transitions)])
        (FA start (list final) states+ transitions+)))))


;; あるstateで入力inputを受けた時に進む先
(define (fa-transitions-from-a-state fa state input)
  (filter-map (lambda (tr)
                (if (and (eq? (trans-state1 tr) state)
                         (eq? (trans-input tr) input))
                    (trans-state2 tr)
                    #f))
              (fa-transitions fa)))

(define (fa-transitions-from-states fa states input)
  (uniq (append-map (cut fa-transitions-from-a-state fa <> input) states)))


;; あるstateで受け付けるepsilon以外の入力
(define (fa-non-eps-inputs-from-a-state fa state)
  (filter-map (lambda (tr)
                (if (and (eq? (trans-state1 tr) state)
                         (not (eq? (trans-input tr) 'eps)))
                    (trans-input tr)
                    #f))
              (fa-transitions fa)))

(define (fa-non-eps-inputs-from-states fa states)
  (sort (uniq (append-map (cut fa-non-eps-inputs-from-a-state fa <>) states))))


;; closure
(define (CL fa state)
  (define (eps-trans state) (fa-transitions-from-a-state fa state 'eps))
  (define ht (make-hash-table 'eq?))
  (hash-table-put! ht state #t)
  (let loop ((states (list state)))
    (if (null? states)
        (hash-table-keys ht)
        (let1 new-states (remove (cut hash-table-exists? ht <>)
                                 (append-map eps-trans states))
          (for-each (cut hash-table-put! ht <> #t) new-states)
          (loop new-states)))))

(define (CLs fa states)
  (uniq (append-map (cut CL fa <>) states)))

;; NFA -> DFA 変換
(define (NFA->DFA nfa)
  (let ([start (fa-start-state nfa)]
        [finals (fa-final-states nfa)]
        [states (fa-states nfa)]
        [transitions (fa-transitions nfa)]
        [S (make-hash-table 'equal?)] ; state groups
        [T (make-hash-table 'equal?)] ; transitions
        [F (make-hash-table 'equal?)] ; final states
        [visited (make-hash-table 'eq?)])

    (define (has-final? states) ;; (intersection states finals) != empty
      (any (cut memq <> states) finals))

    (define (sub from* from-state)
      (unless (hash-table-get visited from-state #f)
        (hash-table-put! visited from-state #t)
        (when *verbose*
          (format #t "  From states ~a:\n" from*))
        (let1 inputs (fa-non-eps-inputs-from-states nfa from*)
          (unless (null? inputs)
            (when *verbose*
              (format #t "    transition inputs from this closure: ~a\n" inputs))
            (for-each (lambda (in)
                        (let* ([to (fa-transitions-from-states nfa from* in)]
                               [to* (CLs nfa to)])
                          (when *verbose*
                            (format #t "     - ~a + ~a -> ~a\n" from* in to*))
                          (let1 to-state (or (hash-table-get S to* #f) (genstate))
                            (hash-table-put! S to* to-state)
                            (hash-table-put! T (Trans from-state in to-state) #t)
                            (when (has-final? to*)
                              (hash-table-put! F to-state #t))
                            (sub to* to-state))))
                      inputs)))))

    (when *verbose*
      (format #t "[NFA->DFA]\n"))
    (let* ([D-start* (CLs nfa (list start))]
           [D-start-state (genstate)])
      (when *verbose*
        (format #t "  CL(~a) = ~a\n" start D-start*))
      (hash-table-put! S D-start* D-start-state)
      (when (has-final? D-start*)
        (hash-table-put! F D-start-state #t))

      (sub D-start* D-start-state)

      (let* ([D-final-states (hash-table-keys F)]
             [D-states (hash-table-values S)]
             [D-transitions (hash-table-keys T)])
        (when *verbose*
          (hash-table-for-each S (lambda (k v)
                                   (format #t "  - new state ~a corresponds ~a\n" v k))))
        (FA D-start-state D-final-states D-states D-transitions)))))

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

(define (NFA->Table fa)
  (define (trans-cmp a b)
    (cond [(equal? a b) #f]
          [(eq? (car a) (car b)) (< (cdr a) (cdr b))]
          [(eq? 'eps (car a)) #t]
          [(eq? 'eps (car b)) #f]
          [else (char<? (car a) (car b))]))
  (let* ([states (fa-states fa)]
         [start (fa-start-state fa)]
         [finals (fa-final-states fa)]
         [trans-table (make-hash-table 'eq?)])
    (map (lambda (state)
           (let1 ht (make-hash-table 'eq?)
             (dolist (tr (fa-transitions fa))
               (when (eq? (trans-state1 tr) state)
                 (let ([in (trans-input tr)]
                       [st2 (trans-state2 tr)])
                   (let1 ls (hash-table-get ht in '())
                     (hash-table-put! ht in (cons st2 ls))))))
             (cons (cons state (if (memq state finals) '* '()))
                   (hash-table-map ht cons))))
         states)))

(define (DFA->Table fa)
  (let ([finals (fa-final-states fa)]
        [trans-table (make-hash-table 'eq?)])
    (dolist (tr (fa-transitions fa))
      (let* ([st1 (trans-state1 tr)]
             [in (trans-input tr)]
             [st2 (trans-state2 tr)]
             [ls (hash-table-get trans-table st1 '())])
        (hash-table-put! trans-table st1 (cons (list in st2) ls))))
    (sort (hash-table-map trans-table
                          (lambda (st1 tr)
                            (cons (cons st1 (if (memq st1 finals) '* '()))
                                  (sort tr (lambda (a b) (char<? (car a) (car b)))))))
          (lambda (a b) (< (caar a) (caar b))))))
