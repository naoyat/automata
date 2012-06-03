(use naoyat.automata.cfg)
(use gauche.interactive)

(use gauche.test)
(test-start "Context-Free Grammar (CFG)")

(define grammar1 '((S -> A B C)
				   (A -> #\a A / ε)
				   (B -> #\b B / ε)
				   (C -> ε)))

(define cfg1 (grammar->CFG grammar1))

(test-section "nullable-symbols")
(test* "grammar 1" '(B C A S) (nullable-symbols cfg1))

;(test-section "removing epsilon-productions")
#;(let1 cfg1a (remove-epsilon-productions cfg1)
  (d cfg1) (d cfg1a))

(test-section "removing ε-productions")
(test* "grammar 1"
	   (grammar->CFG '((S -> A B / A / B)
					   (A -> #\a A / #\a)
					   (B -> #\b B / #\b)))
	   (remove-epsilon-productions cfg1))
(print (remove-epsilon-productions cfg1))

(test-section "removing unit-productions")
(test* "grammar 1"
	   (grammar->CFG '((S -> A B / A / B)
					   (A -> #\a A / #\a)
					   (B -> #\b B / #\b)))
	   (remove-unit-productions
		(remove-epsilon-productions cfg1)))
(print (remove-unit-productions
		(remove-epsilon-productions cfg1)))
(test* ""
	   (grammar->CFG '((S -> A B)
					   (A -> #\a A)
					   (B -> #\e)
					   (C -> #\e)
					   (D -> #\e)
					   (E -> #\e)))
	   (remove-unit-productions
		(grammar->CFG '((S -> A B)
						(A -> #\a A)
						(B -> D)
						(C -> E)
						(D -> C)
						(E -> #\e)))))

(test-section "removing useless productions")
(test* "grammar 1"
	   (grammar->CFG '((S -> A B / A / B)
					   (A -> #\a A / #\a)
					   (B -> #\b B / #\b)))
	   (remove-useless-productions
		(remove-unit-productions
		 (remove-epsilon-productions cfg1))))
(test* ""
	   (grammar->CFG '((S -> C)
					   (A -> #\a A / #\a)
					   (C -> #\c)))
	   (remove-useless-productions
		(grammar->CFG '((S -> A B / C)
						(A -> #\a A / #\a)
						(B -> #\b B)
						(C -> #\c)))))

(test-section "removing unreachable productions")
(test* "grammar 1"
	   (grammar->CFG '((S -> A B / A / B)
					   (A -> #\a A / #\a)
					   (B -> #\b B / #\b)))
	   (remove-unreachable-productions
		(remove-useless-productions
		 (remove-unit-productions
		  (remove-epsilon-productions cfg1)))))
(test* ""
	   (grammar->CFG '((S -> A)
					   (A -> B)
					   (B -> #\b)))
	   (remove-unreachable-productions
		(grammar->CFG '((S -> A)
						(A -> B)
						(B -> #\b)
						(C -> #\c)))))

(test-section "clean-up CFG")
(test* "grammar 1"
	   (grammar->CFG '((S -> A B / A / B)
					   (A -> #\a A / #\a)
					   (B -> #\b B / #\b)))
	   (CFG-cleanup cfg1))

(test-section "CFG->CNF")
(test* "A -> BCDE"
	   (grammar->CNF '((A -> B G1)
					   (G1 -> C G2)
					   (G2 -> D E)
					   (B -> #\b)
					   (C -> #\c)
					   (D -> #\d)
					   (E -> #\e)))
	   (CFG->CNF (grammar->CFG '((A -> B C D E)
								 (B -> #\b)
								 (C -> #\c)
								 (D -> #\d)
								 (E -> #\e)))))
(test* "A -> aBcDe"
	   (grammar->CNF '((A -> G1 G4)
					   (G4 -> B G5)
					   (G5 -> G2 G6)
					   (G6 -> D G3)
					   (G1 -> #\a)
					   (G2 -> #\c)
					   (G3 -> #\e)
					   (B -> #\b)
					   (D -> #\d)))
	   (CFG->CNF (grammar->CFG '((A -> #\a B #\c D #\e)
								 (B -> #\b)
								 (D -> #\d)))))

(test-end)