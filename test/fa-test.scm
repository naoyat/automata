;(use gauche.test)
;(test-start "scanner")

(use naoyat.automata.fa)
(use naoyat.automata.regular)

(define a (Single #\a))
(define b (Single #\b))
(define c (Single #\c))
(fa-prettyprint a "a")
(fa-prettyprint b "b")
(fa-prettyprint c "c")

(define a* (Kleene* a))
(define a+ (Kleene+ a))
(fa-prettyprint a* "a*")
(fa-prettyprint a+ "a+" )
(fa-prettyprint (NFA->DFA a*) "a*")
(fa-prettyprint (NFA->DFA a+) "a+")

(define ab (Concat a b))
(define a+b (Union a b))
(fa-prettyprint ab "ab")
(fa-prettyprint a+b "a+b")

(define abc (Concat a b c))
(define a+b+c (Union a b c))
(fa-prettyprint abc); "abc")
(fa-prettyprint a+b+c); "a+b+c")
(fa-prettyprint (NFA->DFA a+b+c) "a+b+c")

(fa-draw a* "a*" "a-star.gif")
(fa-draw a+ "a+" "a-plus.gif")
(fa-draw abc "abc" "abc.gif")
(fa-draw a+b+c "a+b+c" "a+b+c.gif")
(fa-draw (NFA->DFA a*) "DFA for a*" "a-star-dfa.gif")
(fa-draw (NFA->DFA a+) "DFA for a+" "a-plus-dfa.gif")
(fa-draw (NFA->DFA abc) "DFA for abc" "abc-dfa.gif")
(fa-draw (NFA->DFA a+b+c) "DFA for a+b+c" "a+b+c-dfa.gif")

(define _a+b+c_* (Kleene* a+b+c))
(define _a+b+c_+ (Kleene+ a+b+c))
(fa-draw _a+b+c_* "(a+b+c)*" "a+b+c-star.gif")
(fa-draw (NFA->DFA _a+b+c_*) "DFA for (a+b+c)*" "a+b+c-star-dfa.gif")
(fa-draw _a+b+c_+ "(a+b+c)+" "a+b+c-plus.gif")
(fa-draw (NFA->DFA _a+b+c_+) "DFA for (a+b+c)+" "a+b+c-plus-dfa.gif")

(define rx1 (Concat (Kleene* (Union (Single 1) (Single 0)))
                    (Single 1))) ;; "(1+0)*1"
(fa-draw rx1 "(1+0)*1" "rx1.gif")
(fa-draw (NFA->DFA rx1) "DFA for (1+0)*1" "rx1-dfa.gif")

(define rx2 (Concat (Kleene* (Union (Single 1) (Single 0)))
                    (Single 1)
                    (Kleene* (Union (Single 1) (Single 0))))) ;; "(1+0)*1(1+0)*"
(fa-draw rx2 "(1+0)*1(1+0)*" "rx2.gif")
(fa-draw (NFA->DFA rx2) "DFA for (1+0)*1(1+0)*" "rx2-dfa.gif")
