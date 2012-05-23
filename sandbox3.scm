(require "./re.scm")

(let* ((s "(1+0)*1")
	   (nfa (fa-renum (string->NFA s)))
	   (dfa (fa-renum (NFA->DFA nfa))))
  (fa-draw dfa s "101dfa.gif")
  (map print (NFA->Table nfa))
  (map print (DFA->Table dfa)))
