;;
;; Finite Automata
;;
(define-module naoyat.automata.fa
  (use srfi-1)
  (use gauche.array)
  (use naoyat.automata.production)
  (use naoyat.automata.util)

  (export <FA>
          start-state-of
          final-states-of
          states-of
          transitions-of

          genstate

          <FA-transition>
          object-hash
          write-object

          fa-prettyprint
          fa-inputs
          fa-renum
          fa->dot
          fa-draw
          fa-transitions-from-a-state ;?
          fa-transitions-from-states ;?
          fa-non-eps-inputs-from-a-state ;?
          fa-non-eps-inputs-from-states ;?

          CL ;?
          CLs
          
          NFA->DFA

          NFA->AList
          DFA->AList DFA->Array DFA->Array* DFA->Table
          DFA->Scanner1 DFA->Scanner2 DFA->Scanner
          
          ))
(select-module naoyat.automata.fa)

(define *verbose* #f)

(define-class <FA> ()
  ((start-state  :init-keyword :start-state  :accessor start-state-of)
   (final-states :init-keyword :final-states :accessor final-states-of)
   (states       :init-keyword :states       :accessor states-of)
   (transitions  :init-keyword :transitions  :accessor transitions-of)))

(define (fa-prettyprint fa . opt)
  (format #t "Finite Automaton:\n")
  (format #t "  states: ~a\n" (states-of fa))
  (format #t "    start state: ~a\n" (start-state-of fa))
  (format #t "    final states: ~a\n" (final-states-of fa))
  (format #t "  transitions:\n")
  (for-each (lambda (tr)
              (format #t "    ")
              (print tr))
            (transitions-of fa)))

;;; States
(define (genstate) (gensym)) ;; uniqueなステート番号を振ってくれる何か

;;; Transitions
(define-class <FA-transition> ()
  ((state1 :init-keyword :state1 :accessor state1-of)
   (input  :init-keyword :input  :accessor input-of)
   (state2 :init-keyword :state2 :accessor state2-of)))

(define-method object-hash ((tr <FA-transition>))
  (hash (list 'FAtrans
              (state1-of tr)
              (input-of tr)
              (state2-of tr))))

(define-method write-object ((tr <FA-transition>) port)
  (let ([state1 (state1-of tr)]
        [input  (input-of tr)]
        [state2 (state2-of tr)])
    (format port "~a + " state1)
    (if (epsilon? input)
        (format port "ϵ")
        (format port "'~a'" input))
    (format port " -> ~a" state2)))

;(define (FA? f) (eq? 'FA (car f)))

;; ある要素に与えられた連番を返す。未知の要素に対しては #f を返す。
(define (make-objectid-proc ls . args)
  (let1 ht (apply list->idmap ls args)
    (values
     (lambda (elem) (hash-table-get ht elem #f))
     (list->vector ls))))


(define (fa-inputs fa)
  (uniq (map input-of (transitions-of fa))))

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

    ;; start state を 0 に、final states を後ろの方の番号にする
    (state->id (start-state-of fa))
    (let1 finals (final-states-of fa)
      (let1 others (lset-difference eq? (states-of fa) finals)
        (for-each state->id others))
      (for-each state->id finals))

    (make <FA>
      :start-state (state->id (start-state-of fa))
      :final-states (map state->id (final-states-of fa))
      :states (map state->id (states-of fa))
      :transitions (map (lambda (tr)
                          (make <FA-transition>
                            :state1 (state->id (state1-of tr))
                            :input  (input-of tr)
                            :state2 (state->id (state2-of tr))))
                        (transitions-of fa)))))

;; FAのグラフをdot形式に変換
(define (fa->dot fa name)
  (format #t "digraph \"~a\" {\n" name)
  (format #t "  graph [ rankdir = LR ];\n")
  (format #t "  node [ fontname = \"Courier\", fontsize = 14, shape = circle, width = 0.3, height = 0.3, margin = 0.01 ];\n")
  (format #t "  edge [ fontname = \"Courier\", color = black, weight = 1 ];\n")
  (format #t "  start [ shape = plaintext ];\n")
  (format #t "  start -> ~a;\n" (start-state-of fa))
  (dolist (st (final-states-of fa))
    (format #t "  ~a [ shape = circle, peripheries = 2 ];\n" st))
  (dolist (tr (transitions-of fa))
    (let1 input (input-of tr)
      (when (eq? 'eps input) (set! input "ϵ"))
      (format #t "  ~a -> ~a [ label = \"~a\" ];\n" (state1-of tr) (state2-of tr) input)))
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



;; あるstateで入力inputを受けた時に進む先
(define (transitions-of-from-a-state fa state input)
  (filter-map (lambda (tr)
                (if (and (eq? (state1-of tr) state)
                         (eq? (input-of tr) input))
                    (state2-of tr)
                    #f))
              (transitions-of fa)))

(define (transitions-of-from-states fa states input)
  (uniq (append-map (cut transitions-of-from-a-state fa <> input) states)))


;; あるstateで受け付けるepsilon以外の入力
(define (fa-non-eps-inputs-from-a-state fa state)
  (filter-map (lambda (tr)
                (if (and (eq? (state1-of tr) state)
                         (not (eq? (input-of tr) 'eps)))
                    (input-of tr)
                    #f))
              (transitions-of fa)))

(define (fa-non-eps-inputs-from-states fa states)
  (sort (uniq (append-map (cut fa-non-eps-inputs-from-a-state fa <>) states))))


;; closure
(define (CL fa state)
  (define (eps-trans state) (transitions-of-from-a-state fa state 'eps))
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
  (let ([start (start-state-of nfa)]
        [finals (final-states-of nfa)]
        [states (states-of nfa)]
        [transitions (transitions-of nfa)]
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
                        (let* ([to (transitions-of-from-states nfa from* in)]
                               [to* (CLs nfa to)])
                          (when *verbose*
                            (format #t "     - ~a + ~a -> ~a\n" from* in to*))
                          (let1 to-state (or (hash-table-get S to* #f) (genstate))
                            (hash-table-put! S to* to-state)
                            (hash-table-put! T
                                             (make <FA-transition>
                                               :state1 from-state
                                               :input in
                                               :state2 to-state) #t)
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
        (make <FA>
          :start-state D-start-state
          :final-states D-final-states
          :states D-states
          :transitions D-transitions)))))


(define (NFA->AList fa)
  ;; ( (state1 (in1 st1) (in2 st2) ...)
  ;;   (state2 ...                 ...) )
  (define (trans-cmp a b)
    (cond [(equal? a b) #f]
          [(eq? (car a) (car b)) (< (cdr a) (cdr b))]
          [(eq? 'eps (car a)) #t]
          [(eq? 'eps (car b)) #f]
          [else (char<? (car a) (car b))]))
  (let* ([states (states-of fa)]
         [start (start-state-of fa)]
         [finals (final-states-of fa)]
         [trans-table (make-hash-table 'eq?)])
    (map (lambda (state)
           (let1 ht (make-hash-table 'eq?)
             (dolist (tr (transitions-of fa))
               (when (eq? (state1-of tr) state)
                 (let ([in (input-of tr)]
                       [st2 (state2-of tr)])
                   (let1 ls (hash-table-get ht in '())
                     (hash-table-put! ht in (cons st2 ls))))))
             (cons state (hash-table-map ht cons))))
         states)))

(define (DFA->AList fa)
  ;; ( (state1 (in1 st1) (in2 st2) ...)
  ;;   (state2 ...                 ...) )
  (let ([finals (final-states-of fa)]
        [trans-table (make-hash-table 'eq?)])
    (dolist (tr (transitions-of fa))
      (let* ([st1 (state1-of tr)]
             [in (input-of tr)]
             [st2 (state2-of tr)]
             [ls (hash-table-get trans-table st1 '())])
        (hash-table-put! trans-table st1 (cons (list in st2) ls))))
    (sort (hash-table-map trans-table
                          (lambda (st1 tr)
                            (cons st1 (sort tr (lambda (a b) (char<? (car a) (car b)))))))
          (lambda (a b) (< (car a) (car b))))))

(define (DFA->Array fa max-char-code)
  (let* ([n-states (length (states-of fa))]
         [ar (make-array (shape 0 n-states 0 (+ max-char-code 1)) #f)])
    (dolist (tr (transitions-of fa))
      (array-set! ar (state1-of tr) (char->integer (input-of tr)) (state2-of tr)))
    ar))

(define (DFA->Array* fa)
  (let* ([n-states (length (states-of fa))]
         [inputs (fa-inputs fa)]
         [n-inputs (length inputs)]
         [ar (make-array (shape 0 n-states 0 n-inputs) #f)])
    (receive (idmap rev) (make-objectid-proc inputs)
      (dolist (tr (transitions-of fa))
        (array-set! ar (state1-of tr) (idmap (input-of tr)) (state2-of tr)))
      (values ar idmap rev))))

(define (DFA->Table fa)
  (let* ([n-states (length (states-of fa))]
         [inputs (fa-inputs fa)]
         [n-inputs (length inputs)]
         [ar (make-vector n-states #f)]
         [v (make-vector n-states #f)]
         [pat-id 0]
         [ht (make-hash-table 'equal?)])
    (dotimes (i n-states) (vector-set! ar i (make-vector n-inputs #f)))
    (receive (idmap rev) (make-objectid-proc inputs)
      (dolist (tr (transitions-of fa))
        (vector-set! (vector-ref ar (state1-of tr)) (idmap (input-of tr)) (state2-of tr)))
      (dotimes (i n-states)
        (let1 row (vector-ref ar i)
          (let1 st (hash-table-get ht row #f)
            (if st
                (vector-set! v i st)
                (begin
                  (vector-set! v i pat-id)
                  (hash-table-put! ht row pat-id)
                  (inc! pat-id))))))
      (let1 patterns (make-vector pat-id #f)
        (hash-table-for-each ht (lambda (pat id) (vector-set! patterns id pat)))
        (values patterns v idmap rev) ))))

(define (make-final?-proc fa) (make-set-has?-proc (final-states-of fa) 'eq?))

;; 連想リストを用いたスキャナを作成
(define (DFA->Scanner1 fa)
  (let ([table (DFA->AList fa)]
        [final? (make-final?-proc fa)])
    (lambda (str)
      (let loop ((st 0) (cs (string->list str)))
        (let1 row (assoc st table)
          (if row
              (begin
                (when *verbose* (format #t "cs=~a, st=~a, row=~a\n" cs st row))
                (if (null? cs)
                    (if (final? (car row)) 'accepted 'not-accepted)
                    (let* ([c (car cs)]
                           [asf (assq c (cdr row))])
                      (if asf
                          (let1 to-st (second asf)
                            (when *verbose* (format #t " =>~a ~a\n" c to-st))
                            (loop to-st (cdr cs)))
                          'no-way))))
              'unknown-state))))))

;; 配列を用いたスキャナを作成
(define (DFA->Scanner2 dfa max-char-code)
  (let ([ar (DFA->Array dfa max-char-code)]
        [final? (make-final?-proc dfa)])
    (define (scanner str)
      (let loop ((st 0) (cs (map char->integer (string->list str))))
        (if st
            (if (null? cs)
                (if (final? st) 'accepted 'not-accepted)
                (loop (array-ref ar st (car cs)) (cdr cs)))
            'no-way)))
    scanner))

;; コンパクト化した配列を用いたスキャナを作成
(define (DFA->Scanner dfa)
  (let1 final? (make-final?-proc dfa)
    (receive (patterns table idmap rev) (DFA->Table dfa)
      (define (scanner str)
        (let loop ((st 0) (cs (string->list str)))
          (let1 row (vector-ref patterns (vector-ref table st))
            (if (null? cs)
                (if (final? st) 'accepted 'not-accepted)
                (let1 cid (idmap (car cs))
                  (if cid
                      (loop (vector-ref row cid) (cdr cs))
                      'no-way))))))
      scanner)))
