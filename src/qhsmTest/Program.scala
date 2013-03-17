package qhsmTest

object Program {

	def main(args: Array[String]): Unit  = {
		val stateMachine = new QHsmTest()
		stateMachine.initHsm()
		
		object ReadState extends Enumeration {
			val Consumed, Skipped, End = Value
		}
		
		var readState = ReadState.Consumed
		while (readState != ReadState.End) {
			if (readState == ReadState.Consumed) { print("\nSignal<-") }
			readState = ReadState.Consumed
			Console.in.read.toChar match {
				case 'a' => stateMachine.dispatch(A())
				case 'b' => stateMachine.dispatch(B())
				case 'c' => stateMachine.dispatch(C())
				case 'd' => stateMachine.dispatch(D())
				case 'e' => stateMachine.dispatch(E())
				case 'f' => stateMachine.dispatch(F())
				case 'g' => stateMachine.dispatch(G())
				case 'h' => stateMachine.dispatch(H())
				case 'x' => readState = ReadState.End
				case _ => readState = ReadState.Skipped
			}
		}
		
		println("\nDone")
	}
}