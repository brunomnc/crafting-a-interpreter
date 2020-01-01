package automata

abstract case class AutomataOperations(automata: Automata) {
  val alphabet = automata.alphabet
  val name = automata.name
  var initial: State
  var finals: Seq[State]
  var states: Seq[State]
  var transitions: Seq[Transition]

  def createState(name: Char, i: Boolean, f: Boolean): Unit = {
    if (!states.exists(s => s.name == name)) {
      if (i) {
        initial = State(name)
      } else if (f) {
        finals :+ State(name)
      }
      states :+ State(name)
    }
  }

  def createTransition(state: State, other: State): Transition = {
    if(alphabet.contains(state.name)) {
      transitions.head
    } else {
      transitions :+ Transition(state, other)
      Transition(state, other)
    }
  }

  def addTransition(state: State, transition: Transition): Either[Unit, State] = {
    if(!states.contains(state)) {
      Left(())
    } else {
      state.transitions :+ transition
      Right(state)
    }
  }
}
