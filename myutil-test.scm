(use gauche.test)
(test-start "myutil")
(require "./myutil")

(test-section "split")
(test* "(a b / c d / e)"
	   '((a b) (c d) (e))
	   (split-list '/ '(a b / c d / e)))
(test* "(a b)"
	   '((a b))
	   (split-list '/ '(a b)))

(test-end)
