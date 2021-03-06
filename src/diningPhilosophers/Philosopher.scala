package diningPhilosophers

import qf4scala._
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.event.Logging
import scala.concurrent.duration._

//case class Done() extends QEvent
//case class Hungry() extends QEvent
case class Timeout() extends QEvent

class Philosopher(val id : Int, system : ActorSystem, private val eventBus : QFEventBus) extends QActive(system) {
	val log = Logging(context.system, this)
	val thinkTime : FiniteDuration = 7 seconds;
	val eatTime : FiniteDuration = 5 seconds;
	initHsm()

	override def initializeStateMachine() = {
		log.info("Initializing Philosopher %d.".format(id))
		eventBus.subscribe(self, Eat(0).getClass.getName)
		//system.scheduler.scheduleOnce(thinkTime, self, Timeout())
		initializeState(Thinking) // initial transition
	}

	object Thinking extends QState(TopState) {
		override def onEvent(qEvent: QEvent) : Option[QState] = {
			qEvent match {
				case Entry() => {
					log.info("Philosopher %d is thinking.".format(id))
					Timer.fireIn(thinkTime, Timeout())
				}
				case Timeout() => transitionTo(Hungry)
				case Exit() => {
					log.info("Philosopher %d is exiting thinking state.".format(id))
				}
				case _ => return Some(superState)
			}
			None
		}
	}

	object Hungry extends QState(TopState) {
		override def onEvent(qEvent: QEvent) : Option[QState] = {
			qEvent match {
				case Entry() => {
					log.info("Philosopher %d is hungry.".format(id))
					log.info("Philosopher %d publishes Hungry event.".format(id))
					eventBus.publish(IsHungry(id));
				}
				case Eat(philosopherId) if philosopherId == id => {
					log.info("Philosopher %d receives eat signal.".format(id))
					transitionTo(Eating)
				}
				case Exit() => {
					log.info("Philosopher %d is exiting hungry state.".format(id))
				}
				case _ => return Some(superState)
			}
			None
		}
	}

	object Eating extends QState(TopState) {
		override def onEvent(qEvent: QEvent) : Option[QState] = {
			qEvent match {
				case Entry() => {
					log.info("Philosopher %d is eating.".format(id))
					Timer.fireIn(eatTime, Timeout())
				}
				case Timeout() => transitionTo(Thinking)
				case Exit() => {
					log.info("Philosopher %d is exiting eating state.".format(id))
					log.info("Philosopher %d publishes Done event.".format(id))
					eventBus.publish(IsDone(id));
				}
				case _ => return Some(superState)
			}
			None
		}
	}
}