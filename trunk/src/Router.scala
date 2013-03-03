class Router {
	
	object CatchAll extends QState {
		def apply(event: QEvent) = Some(TopState)
	}
	object Connected extends QState {
		def apply(event: QEvent) = Some(CatchAll)
	}
	object Disconnected extends QState {
		def apply(event: QEvent) = Some(CatchAll)
	}
}
	
case class Connect()
case class Disconnect()