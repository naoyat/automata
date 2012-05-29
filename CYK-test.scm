(use gauche.test)
(test-start "CYK")
(require "./CYK")

(let* ([grammar (CNF->CFG '((S -> A B)
						   (A -> B C / #\a)
						   (B -> A C / #\b)
						   (C -> #\a / #\b)))]
	   [cyk-solver (make-CYK-solver grammar)]
	   [w "ababa"]
	   [cyk-table (cyk-solver w)])
  (test-section "CYK-table")
;;(CFG-pp grammar)
  (test* "CYK table for \"ababa\""
		 '(A)
		 (array-ref cyk-table 0 (- (string-length w) 1))))

(test-end)