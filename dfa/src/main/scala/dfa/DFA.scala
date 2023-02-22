package dfa // leave this line in the file

case class State(label: String)

case class Transition(from: State, to: State, symbol: Char)

class DFA (states: Set[State], transitions: Set[Transition], startState: State, acceptingStates: Set[State]):
    def accepts(s: String) =
        var currentState = startState
        for (c <- s)
            for (transition <- transitions)
                if (currentState == transition.from && c == transition.symbol) {
                    currentState = transition.to
                }
        acceptingStates.contains(currentState)




