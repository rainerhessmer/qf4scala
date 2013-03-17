package diningPhilosophers

import qf4scala._
import akka.event.ActorEventBus
import akka.event.LookupClassification

class QFEventBus extends ActorEventBus with LookupClassification {
	type Event = QEvent
	type Classifier = String

	protected def mapSize() : Int = 5

	protected def classify(event: QEvent): String = {
		println(event.getClass.getName)
		event.getClass.getName
	}

	protected def publish(event: QEvent, subscriber: Subscriber): Unit = {
		subscriber ! event
	}
}
