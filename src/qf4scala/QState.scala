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

abstract class QState(val superState : QState) {
	def onEvent(qEvent: QEvent) : Option[QState] = { Some(superState) }
	override def toString = name
	
	def name = this.getClass.getName
}

object TopState extends QState(null) {
	override def onEvent(qEvent: QEvent) : Option[QState] = None
}