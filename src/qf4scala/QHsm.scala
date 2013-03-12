/*
            qf4scala Library by Dr. Rainer Hessmer

Port of Samek's Quantum Framework to Scala. The implementation takes the liberty
to depart from Miro Samek's code where the specifics of desktop systems
(compared to embedded systems) and Scala seem to warrant a different approach.
 
Reference:
Practical Statecharts in C/C++; Quantum Programming for Embedded Systems
Author: Miro Samek, Ph.D.
http://www.quantum-leaps.com/book.htm

MIT License

Copyright (c) 2013 Dr. Rainer Hessmer

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

package qf4scala
import scala.collection.mutable.ListBuffer

abstract class QHsm {
	private var myState : QState = TopState
	private var mySourceState : QState = TopState
	
	// Is called inside of the function InitHsm to give the deriving class a chance to
	// initialize the state machine.
	protected def InitializeStateMachine()

	// Must only be called once by the client of the state machine to initialize the machine.
	def InitHsm() = {
		assert (myState == TopState, {println("We must be in the top state. The HSM shoule not been executed yet.")})
		var state = myState // save m_StateHandler in a temporary

		InitializeStateMachine() // We call into the deriving class
		// initial transition must go *one* level deep
		var superState = GetSuperState(myState)
		assert (GetSuperState(myState) == Some(state), {println("initial transition must go *one* level deep.")})
		
		state = myState // Note: We only use the temporary
		// variable stateMethod so that we can use Assert statements to ensure
		// that each transition is only one level deep.
		Trigger(state, Entry());
		while(Trigger(state, Init()) match { // init handled?
			case None => {
				assert (GetSuperState(myState) == Some(state), "mistmatched super state.")
				state = myState
				Trigger(state, Entry())
				true
			}
			case Some(x) => false
		}) {}
	}

	// Determines whether the state machine is in the state specified by inquiredState.
	// Note: If the currently active state of a hierarchical state machine is s then it is in the 
	// state s AND all its parent states.
	def IsInState(inquiredState : QState) : Boolean = {
		var optionState : Option[QState] = Some(myState)
		while (true) {
			optionState  match {
				case None => false
				case Some(state) => {
					// do the states match?
					if (state == inquiredState) {
						return true
					}
				}
			}
		}
		error("Needed to trick scala into accepting a return type for the while loop")
		// for details see: http://stackoverflow.com/questions/10802917/scala-whiletrue-type-mismatch-infinite-loop-in-scala
	}

	// Returns the name of the (deepest) state that the state machine is currently in.
	def CurrentStateName = {
		myState.name
	}
	
	// Dispatches the specified event to this state machine
	def Dispatch(qEvent : QEvent) =	{
		// We let the event bubble up the chain until it is handled by a state handler
		mySourceState = myState
		while(mySourceState.onEvent(qEvent) match {
			case Some(x) => mySourceState = x; true;
			case None => false
		}) {}
	}

	private def Trigger(state : QState, qEvent: QEvent) : Option[QState] = {
		state.onEvent(qEvent)
	}
		
	// Retrieves the super state (parent state) of the specified 
	// state by sending it the empty signal. 
	private def GetSuperState(state : QState) : Option[QState] = {
		state.onEvent(Empty())
	}

	// Represents the macro Q_INIT in Miro Samek's implementation
	protected def InitializeState(state : QState) {
		myState = state;
	}

	// Performs a dynamic transition; i.e., the transition path is determined on the fly and not recorded.
	// targetState: The QState to transition to.
	protected def TransitionTo(targetState : QState) = {
		//Debug.Assert(targetState != s_TopState); // can't target 'top' state
		ExitUpToSourceState()
		// This is a dynamic transition. We pass in null instead of a recorder
		TransitionFromSourceToTarget(targetState);
	}

	private def ExitUpToSourceState() = {
		var state = myState
		while(state != mySourceState) {
			//Debug.Assert(stateMethod != null);
			Trigger(state, Exit()) match {
				// state did not handle the Exit signal itself
				case Some(stateToHandleExit) => state = stateToHandleExit
				// state handled the Exit signal. We need to elicit the superstate explicitly.
				case None => {
					GetSuperState(state) match {
						case Some(x) => state = x
						case None => throw new Exception("We need to exist up to a valid state")
					}
				}
			}
		}
	}

	// Handles the transition from the source state to the target state
	// targetState: The QState representing the state to transition to.
	private def TransitionFromSourceToTarget(targetState : QState) = {
		val (statesTargetToLCA, indexFirstStateToEnter) = ExitUpToLCA(targetState)
		TransitionDownToTargetState(targetState, statesTargetToLCA, indexFirstStateToEnter)
	}

	// Determines the transition chain between the target state and the LCA (Least Common Ancestor)
	// and exits up to LCA while doing so.
	// targetState: The QState representing the state to transition to.
	// Returns a tuple consisting of
	// a) statesTargetToLCA: A ListBuffer that holds (in reverse order) the states
	// that need to be entered on the way down to the target state.
	// b) indexFirstStateToEnter: Returns the index in the statesTargetToLCA list
	// that specifies the first state that needs to be entered on the way down to the target state.
	private def ExitUpToLCA(targetState : QState) : (ListBuffer[QState], Int) =
	{
		val statesTargetToLCA = new ListBuffer[QState]()
		statesTargetToLCA.append(targetState)
		var indexFirstStateToEnter : Int = 0
		
		// (a) check my source state == target state (transition to self)
		if(mySourceState == targetState)
		{
			Trigger(mySourceState, Exit());
			return (statesTargetToLCA, indexFirstStateToEnter)
		}
		
		// (b) check my source state == super state of the target state
		val targetSuperState = GetSuperState(targetState) match {
			case None => error("Should never get here")
			case Some(x) => {
				if (mySourceState == x) {
					return (statesTargetToLCA, indexFirstStateToEnter)
				}
				else {
					x // we capture x in targetSuperState
				}
			}
		}
		
		
		// (c) check super state of my source state == super state of target state (most common)
		val sourceSuperState = GetSuperState(mySourceState) match {
			case None => error("Should never get here")
			case Some(x) => {
				if (targetSuperState == x) {
					Trigger(mySourceState, Exit())
					return (statesTargetToLCA, indexFirstStateToEnter)
				}
				else {
					x // we capture x in sourceSuperState
				}
				
			}
		}
		
		// (d) check super state of my source state == target
		if (sourceSuperState == targetState)
		{
			Trigger(mySourceState, Exit())
			indexFirstStateToEnter = -1 // we don't enter the LCA
			return (statesTargetToLCA, indexFirstStateToEnter)
		}
		
		// (e) check rest of my source = super state of super state ... of target state hierarchy
		statesTargetToLCA.append(targetSuperState)
		indexFirstStateToEnter += 1
		
		def walkUpFromTargetState(state : QState) : Boolean = {
			GetSuperState(state) match {
				case None => false
				case Some(superState) => {
					if (mySourceState == superState) {
						true
					}
					else {
						statesTargetToLCA.append(superState)
						indexFirstStateToEnter += 1
						walkUpFromTargetState(superState) // continue to loop
					}
				}
			}
		}
		
		val foundMatch = walkUpFromTargetState(targetSuperState)
		if (foundMatch) {
			return (statesTargetToLCA, indexFirstStateToEnter)
		}
		
		// For both remaining cases we need to exit the source state
		Trigger(mySourceState, Exit())
		
		// (f) check rest of super state of my source state ==
		//     super state of super state of ... target state
		// The array list is currently filled with all the states
		// from the target state up to the top state
		var stateIndex = indexFirstStateToEnter
		while (stateIndex >= 0) {
			if (sourceSuperState == statesTargetToLCA(stateIndex))
			{
				indexFirstStateToEnter = stateIndex - 1
				// Note that we do not include the LCA state itself;
				// i.e., we do not enter the LCA
				return (statesTargetToLCA, indexFirstStateToEnter)
			}
			stateIndex -= 1		
		}
		
		// (g) check each super state of super state ... of my source state ==
		//     super state of super state of ... target state
		def findMatchingSuperSuperSourceAndSuperSuperTarget(state : QState) : (ListBuffer[QState], Int) = {
			if (state == null) {
				// We should never get here
				throw new Exception("Malformed Hierarchical State Machine"); 
			}
			
			var stateIndex = indexFirstStateToEnter
			while (stateIndex >= 0) {
				if (state == statesTargetToLCA(stateIndex))
				{
					indexFirstStateToEnter = stateIndex - 1
					// Note that we do not include the LCA state itself;
					// i.e., we do not enter the LCA
					return (statesTargetToLCA, indexFirstStateToEnter)
				}
				stateIndex -= 1
			}
			
			// we move one level up
			Trigger(state, Exit())
			GetSuperState(state) match {
				case None => findMatchingSuperSuperSourceAndSuperSuperTarget(null)
				case Some(superState) => findMatchingSuperSuperSourceAndSuperSuperTarget(superState)
			}
		}
		
		findMatchingSuperSuperSourceAndSuperSuperTarget(sourceSuperState)
	}
		
	private def TransitionDownToTargetState(
		targetState : QState, 
		statesTargetToLCA : ListBuffer[QState], 
		indexFirstStateToEnter : Int) =
	{
		// we enter the states in the passed in array in reverse order
		var stateIndex = indexFirstStateToEnter
		while (stateIndex >= 0) {
			Trigger(statesTargetToLCA(stateIndex), Entry())
			stateIndex -= 1
		}
		
		myState = targetState
		
		// At last we are ready to initialize the target state.
		// If the specified target state handles init then the effective
		// target state is deeper than the target state specified in
		// the transition.
		var currentState = targetState
		while (Trigger(currentState, Init()) match {
			// Initial transition must be one level deep
			//Debug.Assert(targetStateMethod == GetSuperStateMethod(m_MyStateMethod));
			case None => {
				// the state handled the init signal. Initial transition must be one level deep
				assert (GetSuperState(myState) == Some(currentState), "current state must be super state of myState.")
				currentState = myState
				Trigger(currentState, Entry())
				true
			}
			case Some(state) => false
		}) {}
	}
}