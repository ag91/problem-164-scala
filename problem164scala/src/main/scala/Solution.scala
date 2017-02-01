package problem164scala

case class State[S](state: S)

case class Word[W](word: W)

case class Transition[S,W](from: State[S], label: Word[W], to: State[S])

case class DFA[S,W](states: Set[State[S]], alphabet: Set[Word[W]], start: State[S], accepts: Set[State[S]], transitions: Set[Transition[S,W]])

object Solution {

def solve(_dfa: => DFA[String,String]): Stream[String] = {
lazy val dfa = _dfa
def collectFinished(_starts: Seq[(State[String],String)], _acc: Seq[String]) : Seq[String] = {
lazy val starts = _starts
def f1(acc: Seq[String], kv: (State[String],String)) : Seq[String] = {
kv match {
case (k,v) => {
dfa.accepts.contains(k) match {
case true => // we can return the string
acc :+ v
case false => //we cannot return the string
acc
}
}
}
}
starts.foldLeft(_acc)(f1)      
}

def leaveOnlyTransitionable(_starts: Seq[(State[String],String)]): Seq[(State[String],String)] = {
// get all accepted states from which no transition start
val ss = dfa.accepts.filterNot(s => dfa.transitions.exists(t => t.from == s))
// we do not want the pairs with a state in [ss]
_starts.filterNot(kv => ss.contains(kv._1))
}

def applyAvailableTransitions(_starts: Seq[(State[String],String)]): Seq[(State[String],String)] = {
def f1(acc: Seq[(State[String],String)], kv: (State[String],String)): Seq[(State[String],String)] = {
// get all transitions starting from k
val kTransitions = dfa.transitions.filter(t => t.from == kv._1)
// apply transition
val pacc = kTransitions.map(t => (t.to,kv._2 + t.label.word))
acc ++ pacc
}
_starts.foldLeft(List().view:(Seq[(State[String],String)]))(f1)
}

def solve1(_starts: Seq[(State[String],String)], _acc: Seq[String]): (Seq[(State[String],String)],Seq[String]) = {
lazy val starts = _starts
// we collect all the strings that are finished
val acc = collectFinished(starts,List().view)
// then we clean all the states that are accepted and that
// cannot run a transition from _starts
val starts0 = leaveOnlyTransitionable(starts)
// then we update [starts0] by applying the transitions
val starts1 = applyAvailableTransitions(starts0)
(starts1,acc)
}


def recsolve1(_starts: Seq[(State[String],String)], _acc: Seq[String]): Stream[String] = {
val (starts,acc) = solve1(_starts,_acc)
if (starts.isEmpty) {
acc.toStream
} else {acc.toStream #::: recsolve1(starts,acc.view)}
}

recsolve1(List((dfa.start,"")), List())

}
}
