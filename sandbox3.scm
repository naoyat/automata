(require "./re.scm")

(let* ((s "(1+0)*1")
	   (nfa (fa-renum (string->NFA s)))
	   (dfa (fa-renum (NFA->DFA nfa))))
  (fa-draw dfa s "101dfa.gif")
  (map print (FA->Table nfa))
  (map print (FA->Table dfa)))
