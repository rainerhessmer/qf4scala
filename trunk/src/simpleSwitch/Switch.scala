package simpleSwitch
import qf4scala._

class Switch extends QHsm {
	
	override def InitializeStateMachine() = {
		InitializeState(CatchAll) // initial transition
	}
	
	object CatchAll extends QState(TopState) {
		override def onEvent(qEvent: QEvent) : Option[QState] = {
			qEvent match {
				case Init() => InitializeState(Connected)
				case Entry() => println("Entering CatchAll")
				case Exit() => println("Exiting CatchAll")
				case _ => return Some(superState)
			}
			None
		}
	}
	object Connected extends QState(CatchAll) {
		override def onEvent(qEvent: QEvent) : Option[QState] = {
			qEvent match {
				case Entry() => println("Entering Connected")
				case Exit() => println("Exiting Connected")
				case Disconnect() => TransitionTo(Disconnected)
				case _ => return Some(superState)
			}
			None
		}
	}
	object Disconnected extends QState(CatchAll) {
		override def onEvent(qEvent: QEvent) : Option[QState] = {
			qEvent match {
				case Entry() => println("Entering Disconnected")
				case Exit() => println("Exiting Disconnected")
				case Connect() => TransitionTo(Connected)
				case _ => return Some(superState)
			}
			None
		}
	}
}

case class Connect() extends QEvent
case class Disconnect() extends QEvent