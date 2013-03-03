
object MyProgram {

	def main(args: Array[String]): Unit = {
		var signal = new Init
		println(signal)
		
		val router = new Router()
		router.CatchAll.onEvent(Entry())
		
	}
	
	def foo() : Option[Int] = if (true) Some(4) else None
	
	//def connect(signal : QSignal) : Option[QSignal => Function1[QSignal, QState]]

}