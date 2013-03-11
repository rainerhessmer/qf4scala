package qhsmTest

object Program {

	def main(args: Array[String]): Unit  = {
		val stateMachine = new QHsmTest()
		stateMachine.InitHsm()
		print("\nSignal<-")
		
		var ok = true
		while (ok) {
			Console.in.read.toChar match {
				case 'a' => stateMachine.Dispatch(A())
				case 'b' => stateMachine.Dispatch(B())
				case 'c' => stateMachine.Dispatch(C())
				case 'd' => stateMachine.Dispatch(D())
				case 'e' => stateMachine.Dispatch(E())
				case 'f' => stateMachine.Dispatch(F())
				case 'g' => stateMachine.Dispatch(G())
				case 'h' => stateMachine.Dispatch(H())
				case _ => ok = false
			}
		}
	}
}