import qf4scala._

object MyProgram {

	def main(args: Array[String]): Unit = {
		var signal = new Init
		println(signal)
		
		val switch = new Switch()
		switch.InitHsm()
		switch.Dispatch(Disconnect())
		switch.Dispatch(Connect())
		//switch.CatchAll.onEvent(Entry())
		
	}
	
	
	//def connect(signal : QSignal) : Option[QSignal => Function1[QSignal, QState]]

}