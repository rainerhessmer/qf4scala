package qf4scala

abstract class QState {
	def onEvent(qEvent: QEvent) : Option[QState]
	override def toString = name
	
	def name = this.getClass.getName
}

object TopState extends QState {
	override def onEvent(qEvent: QEvent) : Option[QState] = None 
}