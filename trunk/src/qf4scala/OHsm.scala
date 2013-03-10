package qf4scala
import scala.collection.mutable.ListBuffer

abstract class QHsm {
	private var myState : QState = TopState
	private var mySourceState : QState = TopState
	
	// Is called inside of the function Init to give the deriving class a chance to
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
				assert (GetSuperState(myState) == Some(state), {println("mistmatched super state.")})
				state = myState
				Trigger(state, Entry())
				true
			}
			case Some(x) => false
		}) {}
	}

	/// Determines whether the state machine is in the state specified by <see paramref="inquiredState"/>.
	/// </summary>
	/// <param name="inquiredState">The state to check for.</param>
	/// <returns>
	/// <see langword="true"/> if the state machine is in the specified state; 
	/// <see langword="false"/> otherwise.
	/// </returns>
	/// <remarks>
	/// If the currently active state of a hierarchical state machine is s then it is in the 
	/// state s AND all its parent states.
	/// </remarks>
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

	/// <summary>
	/// Returns the name of the (deepest) state that the state machine is currently in.
	/// </summary>
	def CurrentStateName
	{
		myState.name
	}
	
	/// <summary>
	/// Dispatches the specified event to this state machine
	/// </summary>
	/// <param name="qEvent">The <see cref="IQEvent"/> to dispatch.</param>
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
		
	///<summary>
	/// Retrieves the super state (parent state) of the specified 
	/// state by sending it the empty signal. 
	///</summary>
	private def GetSuperState(state : QState) : Option[QState] = {
		state.onEvent(Empty())
	}

	/// <summary>
	/// Represents the macro Q_INIT in Miro Samek's implementation
	/// </summary>
	protected def InitializeState(state : QState) {
		myState = state;
	}

	/// <summary>
	/// Performs a dynamic transition; i.e., the transition path is determined on the fly and not recorded.
	/// </summary>
	/// <param name="targetState">The <see cref="QState"/> to transition to.</param>
	protected def TransitionTo(targetState : QState) = {
		//Debug.Assert(targetState != s_TopState); // can't target 'top' state
		ExitUpToSourceState()
		// This is a dynamic transition. We pass in null instead of a recorder
		TransitionFromSourceToTarget(targetState);
	}

	private def ExitUpToSourceState() = {
		var state : QState = myState
		while(state != mySourceState) {
			//Debug.Assert(stateMethod != null);
			Trigger(state, Exit()) match {
				// state did not handle the Exit signal itself
				case Some(stateToHandleExit) => state = stateToHandleExit
				// state handled the Exit signal. We need to elicit
				// the superstate explicitly.
				case None => {
					GetSuperState(state) match {
						case Some(x) => state = x
						case None => throw new Exception("We need to exist up to a valid state")
					}
				}
			}
		}
	}

	/// <summary>
	/// Handles the transition from the source state to the target state without the help of a previously
	/// recorded transition chain.
	/// </summary>
	/// <param name="targetState">The <see cref="MethodInfo"/> representing the state method to transition to.</param>
	/// <param name="recorder">An instance of <see cref="TransitionChainRecorder"/> or <see langword="null"/></param>
	/// <remarks>
	/// Passing in <see langword="null"/> as the recorder means that we deal with a dynamic transition.
	/// If an actual instance of <see cref="TransitionChainRecorder"/> is passed in then we deal with a static
	/// transition that was not recorded yet. In this case the function will record the transition steps
	/// as they are determined.
	/// </remarks>
	private def TransitionFromSourceToTarget(targetState : QState) = {
		val (statesTargetToLCA, indexFirstStateToEnter) = ExitUpToLCA(targetState)
		TransitionDownToTargetState(targetState, statesTargetToLCA, indexFirstStateToEnter)
	}

	/// <summary>
	/// Determines the transition chain between the target state and the LCA (Least Common Ancestor)
	/// and exits up to LCA while doing so.
	/// </summary>
	/// <param name="targetState">The target state method of the transition.</param>
	/// <param name="statesTargetToLCA">An <see cref="ArrayList"/> that holds (in reverse order) the states
	/// that need to be entered on the way down to the target state.
	/// Note: The index of the first state that needs to be entered is returned in 
	/// <see paramref="indexFirstStateToEnter"/>.</param>
	/// <param name="indexFirstStateToEnter">Returns the index in the array <see cparamref="statesTargetToLCA"/>
	/// that specifies the first state that needs to be entered on the way down to the target state.</param>
	/// <param name="recorder">An instance of <see cref="TransitionChainRecorder"/> if the transition chain
	/// should be recorded; <see langword="null"/> otherwise.</param>
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
		
		
		// (c) check super state of my source state == super state of target state
		// (most common)
		var sourceSuperState = GetSuperState(mySourceState) match {
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
		var state = targetSuperState
		while (GetSuperState(state) match {
			case None => false
			case Some(superState) => {
				if (mySourceState == superState) {
					return (statesTargetToLCA, indexFirstStateToEnter)
				}
				else {
					statesTargetToLCA.append(superState)
					indexFirstStateToEnter += 1
					state = superState
					true // continue to loop
				}
			}
		}) {}
		
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
		state = sourceSuperState
		while (state != null) {
			stateIndex = indexFirstStateToEnter
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
			Trigger(state, Exit())
		}
		
		// We should never get here
		throw new Exception("Mal formed Hierarchical State Machine"); 
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