(require "./re.scm")

(use gauche.test)
(test-start "scanner")

;(define-macro (time* n proc)
;  `(time (dotimes (i ,n) ,proc)))

(define-macro (test-scanner scanner label)
  `(begin
	 (test-section ,label)
	 (test* "10101" 'accepted (,scanner "10101"))
	 (test* "10100" 'not-accepted (,scanner "10100"))
	 (test* "00101" 'accepted (,scanner "00101"))
	 (test* "00100" 'not-accepted (,scanner "00100"))
	 (test* "1" 'accepted (,scanner "1"))
	 (test* "(empty string)" 'not-accepted (,scanner ""))
	 ))


(let* ((s "(1+0)*1")
	   (nfa (fa-renum (string->NFA s)))
	   (dfa (fa-renum (NFA->DFA nfa))))
  ;(fa-draw dfa s "101dfa.gif")
#|
  (map print (NFA->AList nfa))
  (map print (DFA->AList dfa))
|#
;  (time* 1000 (DFA->Scanner1 dfa))    ;  26 usec + 11
;  (time* 1000 (DFA->Scanner2 dfa 50)) ; 111 usec + 47
;  (time* 1000 (DFA->Scanner1 dfa))    ;  24 usec + 9

  (let1 scanner (DFA->Scanner1 dfa)
	(test-scanner scanner "Scanner1 - Assoc List版"))

  (let1 scanner (DFA->Scanner2 dfa 50)
	(test-scanner scanner "Scanner2 - Array版"))

  ;; (print (DFA->Array* dfa))
  ;; コンパクト版
  (let1 scanner (DFA->Scanner dfa)
	(test-scanner scanner "Scanner - コンパクト版"))

  )

(test-end)