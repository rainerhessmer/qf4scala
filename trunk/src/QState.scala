//abstract class QState extends Function1[QSignal, Option[QState]] {
//}

//abstract class QState extends (QSignal => Option[QState]) {
//}
//
//
//object TopState extends QState
//{
//	def apply(signal: QSignal) = None
//}

abstract class QState {
	def onEvent(qEvent: QEvent) : Option[QState]
	override def toString = name
	
	def name = this.getClass.getName
}

object TopState extends QState {
	override def onEvent(qEvent: QEvent) : Option[QState] = None 
}