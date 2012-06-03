(use naoyat.automata.regular)
(use naoyat.automata.fa)

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
