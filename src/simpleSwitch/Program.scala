package simpleSwitch
import qf4scala._

object Program {

	def main(args: Array[String]): Unit = {
		var signal = new Init
		println(signal)
		
		val switch = new Switch()
		switch.InitHsm()
		switch.Dispatch(Disconnect())
		switch.Dispatch(Connect())
	}
}