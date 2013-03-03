abstract class QEvent {
	override def toString = this.getClass.getName
}

case class Empty() extends QEvent // used to retrieve super state; must not be used externally
case class Init() extends QEvent
case class Entry() extends QEvent
case class Exit() extends QEvent

