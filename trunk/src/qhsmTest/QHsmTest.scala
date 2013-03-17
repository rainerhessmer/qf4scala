package qhsmTest
import qf4scala._

class QHsmTest extends QHsm {
	var foo = false
	
	override def initializeStateMachine() = {
		initializeState(S0) // initial transition
	}

	object S0 extends QState(TopState) {
		override def onEvent(qEvent: QEvent) : Option[QState] = {
			qEvent match {
				case Init() => print("s0-INIT;"); initializeState(S1)
				case Entry() => print("s0-ENTRY;")
				case Exit() => print("s0-EXIT;")
				case E() => print("s0-E;"); transitionTo(S211)
				case _ => return Some(superState)
			}
			None
		}
	}
	object S1 extends QState(S0) {
		override def onEvent(qEvent: QEvent) : Option[QState] = {
			qEvent match {
				case Init() => print("s1-INIT;"); initializeState(S11)
				case Entry() => print("s1-ENTRY;")
				case Exit() => print("s1-EXIT;")
				case A() => print("s1-A;"); transitionTo(S1)
				case B() => print("s1-B;"); transitionTo(S11)
				case C() => print("s1-C;"); transitionTo(S2)
				case D() => print("s1-D;"); transitionTo(S0)
				case F() => print("s1-F;"); transitionTo(S211)
				case _ => return Some(superState)
			}
			None
		}
	}
	object S11 extends QState(S1) {
		override def onEvent(qEvent: QEvent) : Option[QState] = {
			qEvent match {
				case Entry() => print("s11-ENTRY;")
				case Exit() => print("s11-EXIT;")
				case G() => print("s1-G;"); transitionTo(S211)
				case H() => if (foo) {
					print("s11-H;")
					foo = false
				}
				else {
					return Some(superState)
				}
				case _ => return Some(superState)
			}
			None
		}
	}
	object S2 extends QState(S0) {
		override def onEvent(qEvent: QEvent) : Option[QState] = {
			qEvent match {
				case Init() => print("s2-INIT;"); initializeState(S21)
				case Entry() => print("s2-ENTRY;")
				case Exit() => print("s2-EXIT;")
				case C() => print("s2-C;"); transitionTo(S1)
				case F() => print("s2-F;"); transitionTo(S11)
				case _ => return Some(superState)
			}
			None
		}
	}
	object S21 extends QState(S2) {
		override def onEvent(qEvent: QEvent) : Option[QState] = {
			qEvent match {
				case Init() => print("s21-INIT;"); initializeState(S211)
				case Entry() => print("s21-ENTRY;")
				case Exit() => print("s21-EXIT;")
				case B() => print("s21-B;"); transitionTo(S211)
				case H() => if (!foo) {
					print("s21-H;")
					foo = true
					transitionTo(S21)
				}
				else {
					return Some(superState)
				}
				case _ => return Some(superState)
			}
			None
		}
	}
	object S211 extends QState(S21) {
		override def onEvent(qEvent: QEvent) : Option[QState] = {
			qEvent match {
				case Entry() => print("s211-ENTRY;")
				case Exit() => print("s211-EXIT;")
				case D() => print("s211-D;"); transitionTo(S21)
				case G() => print("s211-G;"); transitionTo(S0)
				case _ => return Some(superState)
			}
			None
		}
	}
}

case class A() extends QEvent
case class B() extends QEvent
case class C() extends QEvent
case class D() extends QEvent
case class E() extends QEvent
case class F() extends QEvent
case class G() extends QEvent
case class H() extends QEvent