package automata

case class State(name: Char, var transitions: Seq[Transition] = Nil)
