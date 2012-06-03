(use naoyat.automata.fa)
(use naoyat.automata.regular)

(set! *ullman* #f)

(let* ((s "(1+0)*1")
       (nfa (string->NFA s))
       (dfa (NFA->DFA nfa)))
  (fa-draw nfa s "101.gif")
  (fa-draw dfa s "101D.gif"))

(let* ((s "(10)*+0")
       (nfa (string->NFA s))
       (dfa (NFA->DFA nfa)))
  (fa-draw nfa s "10-0.gif")
  (fa-draw dfa s "10-0D.gif"))

(fa-draw (string->NFA "abc") "abc" "abc.gif")
(fa-draw (string->NFA "a*b") "a*b" "a-star-b.gif")
(fa-draw (string->NFA "(a+b)(c+d)") "(a+b)(c+d)" "abcd.gif")
(fa-draw (string->NFA "(a+b)(c+d)") "(a+b)(c+d)" "abcd.gif")
(fa-draw (NFA->DFA (string->NFA "(a+b)(c+d)")) "DFA for (a+b)(c+d)" "abcd-dfa.gif")
(fa-draw (string->NFA "(a+b)*c(d+e)") "(a+b)*c(d+e)" "abcde1.gif")
(fa-draw (NFA->DFA (string->NFA "(ab+cd)*ef(gh+ij)")) "(ab+cd)*ef(gh+ij)" "abcde2.gif")

(fa-draw (regexp->NFA "abc") "abc" "abc.gif")
(fa-draw (regexp->NFA "a*b") "a*b" "a-star-b.gif")
(fa-draw (regexp->NFA "[ab][cd]") "[ab][cd]" "abcd.gif")
(fa-draw (regexp->NFA "[a-z]?") "[a-z]?" "a-z.gif")
(fa-draw (regexp->NFA "[ab]*c[de]") "[ab]*c[de]" "abcde1.gif")
(fa-draw (NFA->DFA (regexp->NFA "(ab|cd)*ef(gh|ij)")) "(ab|cd)*ef(gh|ij)" "abcde2.gif")

(set! *ullman* #t)
(fa-draw (string->NFA "a*") "a*" "a-star-ullman.gif")
(set! *ullman* #f)
(fa-draw (string->NFA "a*") "a*" "a-star-aiken.gif")

(fa-draw (string->NFA "(a+b)*c") "(a+b)*c" "abc1.gif")
(fa-draw (regexp->NFA "[ab]*c") "[ab]*c" "abc2.gif")

(fa-draw (regexp->NFA "[ab][c-e]") "[ab][c-e]" "abc-e.gif")
(fa-draw (NFA->DFA (regexp->NFA "[ab][c-e]")) "DFA for [ab][c-e]" "abc-e-dfa.gif")
