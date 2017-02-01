package problem164scala

import org.scalatest._

class GivenTests extends FunSuite with Matchers {

def makeDFA(states: Set[String], alphabet: Set[String], start: String, accepts: Set[String], transitions: Set[(String,String,String)]) = {
val states1 = states.map((x:String) => State(x))
val alphabet1 = alphabet.map((x:String) => Word(x))
val start1 = State(start)
val accepts1 = accepts.map((x:String) => State(x))
val transitions1 = transitions.map((flt:(String,String,String)) => Transition(State(flt._1), Word(flt._2), State(flt._3)))
DFA(states1,alphabet1,start1,accepts1,transitions1)
}

test("produce stream {'a' 'ab' 'abc'} for the given DFA") {
val states = Set("q0","q1","q2","q3")
val alphabet = Set("a","b","c")
val start = "q0"
val accepts = Set("q1","q2","q3")
val transitions = Set(("q0","a","q1"),("q1","b","q2"),("q2","c","q3"))
val dfa = makeDFA(states,alphabet,start,accepts,transitions)
Solution.solve(dfa) should be (Stream("a","ab","abc"))
}

test("produce a stream {'hi' 'hey' 'hello'} for the given DFA") {
val states = Set("q0","q1","q2","q3","q4","q5","q6","q7")
val alphabet = Set("e","h","i","l","o","y")
val start = "q0"
val accepts = Set("q2","q4","q7")
val transitions = Set(("q0","h","q1"),("q1","i","q2"),("q1","e","q3"),("q3","l","q5"),("q3","y","q4"),("q5","l","q6"),("q6","o","q7"))
val dfa = makeDFA(states,alphabet,start,accepts,transitions)
Solution.solve(dfa) should be (Stream("hi","hey","hello"))
}


test("produce stream created by list comprehension for the given DFA") {
val states = Set("q0","q1","q2","q3","q4")
val alphabet = Set("v","w","x","y","z")
val start = "q0"
val accepts = Set("q4")
val transitions = Set(("q0","v","q1"),("q0","w","q1"),("q0","x","q1"),("q0","y","q1"),("q0","z","q1"),
         ("q1","v","q2"),("q1","w","q2"),("q1","x","q2"),("q1","y","q2"),("q1","z","q2"),
         ("q2","v","q3"),("q2","w","q3"),("q2","x","q3"),("q2","y","q3"),("q2","z","q3"),
         ("q3","v","q4"),("q3","w","q4"),("q3","x","q4"),("q3","y","q4"),("q3","z","q4"))
val dfa = makeDFA(states,alphabet,start,accepts,transitions)
val s = "vwxyz"
val result =
(for
(v <- 0 until 5;
w <- 0 until 5;
x <- 0 until 5;
y <- 0 until 5) 
yield
(s(v)::s(w)::s(x)::s(y)::Nil).mkString
)
Solution.solve(dfa).sorted should be (result.sorted.toStream)
}

test("produce stream 01 for the given DFA and test by property") {
val states = Set("q0","q1")
val alphabet = Set("0","1")
val start = "q0"
val accepts = Set("q0")
val transitions = Set(("q0","0","q0"),("q0","1","q1"),
         ("q1","0","q1"),("q1","1","q0"))
val dfa = makeDFA(states,alphabet,start,accepts,transitions)
val stream = Solution.solve(dfa)
val res = stream.take(2000)
val pred1 = res.forall(s => s.matches("""0*(?:10*10*)*""".r.toString()))
val pred2 = res == res.distinct
(pred1 && pred2) should be (true)
}


test("produce stream nm for the given DFA and test by property") {
val states = Set("q0","q1")
val alphabet = Set("n","m")
val start = "q0"
val accepts = Set("q1")
val transitions = Set(("q0","n","q0"),("q0","m","q1"))
val dfa = makeDFA(states,alphabet,start,accepts,transitions)
val stream = Solution.solve(dfa)
val res = stream.take(2000)
val pred1 = res.forall(s => s.matches("""n*m""".r.toString()))
val pred2 = res == res.distinct
(pred1 && pred2) should be (true)
}


test("produce stream ilompt for the given DFA and test by property") {
val states = Set("q0","q1","q2","q3","q4","q5","q6","q7","q8","q9")
val alphabet = Set("i","l","o","m","p","t")
val start = "q0"
val accepts = Set("q5","q8")
val transitions = Set(("q0","l","q1"),
         ("q1","i","q2"),("q1","o","q6"),
         ("q2","m","q3"),
         ("q3","i","q4"),
         ("q4","t","q5"),
         ("q6","o","q7"),
         ("q7","p","q8"),
         ("q8","l","q9"),
         ("q9","o","q6"))
val dfa = makeDFA(states,alphabet,start,accepts,transitions)
val stream = Solution.solve(dfa)
val res = stream.take(2000)
val pred1 = res.forall(s => s.matches("""limit|(?:loop)+""".r.toString()))
val pred2 = res == res.distinct
(pred1 && pred2) should be (true)
}
}
