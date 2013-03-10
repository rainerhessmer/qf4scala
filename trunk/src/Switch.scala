import qf4scala._

class Switch extends QHsm {
	
//	def this() {
//		this()
//		InitHsm()
//	}
	override def InitializeStateMachine() = {
		InitializeState(CatchAll) // initial transition
	}
	

	object CatchAll extends QState {
		override def onEvent(qEvent: QEvent) : Option[QState] = {
			qEvent match {
				case Init() => InitializeState(Connected); return None
				case Entry() => println("Entering CatchAll"); return None
				case Exit() => println("Exiting CatchAll"); return None
				case _ => return Some(TopState)
			}
			
		}
	}
	object Connected extends QState {
		override def onEvent(qEvent: QEvent) : Option[QState] = {
			qEvent match {
				case Entry() => println("Entering Connected"); return None
				case Exit() => println("Exiting Connected"); return None
				case Disconnect() => TransitionTo(Disconnected); return None
				case _ => return Some(CatchAll)
			}
		}
	}
	object Disconnected extends QState {
		override def onEvent(qEvent: QEvent) : Option[QState] = {
			qEvent match {
				case Entry() => println("Entering Disconnected"); return None
				case Exit() => println("Exiting Disconnected"); return None
				case Connect() => TransitionTo(Connected); return None
				case _ => return Some(CatchAll)
			}
		}
	}

}

case class Connect() extends QEvent
case class Disconnect() extends QEvent