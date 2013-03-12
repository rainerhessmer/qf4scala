package qhsmTest

object Program {

	def main(args: Array[String]): Unit  = {
		val stateMachine = new QHsmTest()
		stateMachine.InitHsm()
		
		object ReadState extends Enumeration {
			val Consumed, Skipped, End = Value
		}
		
		var readState = ReadState.Consumed
		while (readState != ReadState.End) {
			if (readState == ReadState.Consumed) { print("\nSignal<-") }
			readState = ReadState.Consumed
			Console.in.read.toChar match {
				case 'a' => stateMachine.Dispatch(A())
				case 'b' => stateMachine.Dispatch(B())
				case 'c' => stateMachine.Dispatch(C())
				case 'd' => stateMachine.Dispatch(D())
				case 'e' => stateMachine.Dispatch(E())
				case 'f' => stateMachine.Dispatch(F())
				case 'g' => stateMachine.Dispatch(G())
				case 'h' => stateMachine.Dispatch(H())
				case 'x' => readState = ReadState.End
				case _ => readState = ReadState.Skipped
			}
		}
		
		println("\nDone")
	}
}