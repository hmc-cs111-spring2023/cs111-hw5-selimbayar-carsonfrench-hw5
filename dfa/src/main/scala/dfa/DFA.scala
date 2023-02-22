package dfa // leave this line in the file

case class State(label: String)

case class Transition(from: State, to: State, symbol: Char)

class DFA (val states: Set[State], val transitions: Set[Transition], val startState: State, val acceptingStates: Set[State]):
    def accepts(s: String) =
        var currentState = startState
        var validTransition = transitions.head
        var canTranslate = false
        for (c <- s)
            canTranslate = false
            for (transition <- transitions)
                if (currentState == transition.from && c == transition.symbol) {
                    validTransition = transition
                    canTranslate = true
                }
            if (canTranslate) {
                currentState = validTransition.to
            }
        acceptingStates.contains(currentState)




