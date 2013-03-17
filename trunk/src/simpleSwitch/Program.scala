package simpleSwitch
import qf4scala._

object Program {

	def main(args: Array[String]): Unit = {
		val switch = new Switch()
		switch.initHsm()
		switch.dispatch(Disconnect())
		switch.dispatch(Connect())
	}
}