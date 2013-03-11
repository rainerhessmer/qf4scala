package simpleSwitch
import qf4scala._

object Program {

	def main(args: Array[String]): Unit = {
		val switch = new Switch()
		switch.InitHsm()
		switch.Dispatch(Disconnect())
		switch.Dispatch(Connect())
	}
}