class Router {
	
	object CatchAll extends QState {
		override def onEvent(qEvent: QEvent) : Option[QState] = Some(TopState)
	}
	object Connected extends QState {
		override def onEvent(qEvent: QEvent) : Option[QState] = Some(CatchAll)
	}
	object Disconnected extends QState {
		override def onEvent(qEvent: QEvent) : Option[QState] = Some(CatchAll)
	}
}
	
case class Connect()
case class Disconnect()