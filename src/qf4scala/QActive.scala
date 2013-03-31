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

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.ActorRef
import akka.actor.Props
import akka.actor.Cancellable
import akka.event.Logging
import scala.concurrent.duration._

abstract class QActive(protected val system : ActorSystem) extends QHsm with Actor {
	protected object Timer{
		private var timer : Option[Cancellable] = None

		//Use the system's dispatcher as ExecutionContext
	    import system.dispatcher

		def fireIn(delay: FiniteDuration, message: Any) : Unit = {
			disarm()
			timer = Some(system.scheduler.scheduleOnce(delay, self, message))
		}

		def fireEvery(delay: FiniteDuration, message: Any) : Unit = {
			disarm()
			timer = Some(system.scheduler.schedule(0 seconds, delay, self, message))
		}
		
		def disarm() = {
			for(t <- timer) { t.cancel() }
		}	
	}

	//Use the system's dispatcher as ExecutionContext
    import system.dispatcher
    	
	def receive = {
		case x : QEvent => dispatch(x)
		case whatever => println("unhandled receive value: " + whatever)
	}
}