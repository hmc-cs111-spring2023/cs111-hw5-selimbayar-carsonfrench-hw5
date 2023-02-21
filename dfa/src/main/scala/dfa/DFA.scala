package dfa // leave this line in the file

case class State(label: String):
    override def toString = label

case class Transition(from: State, symbol: Char, to: State)

class DFA (states: Set[State], transitions: Set[Transition], startState: State, acceptingStates: Set[State], )


