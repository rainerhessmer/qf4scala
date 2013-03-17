package diningPhilosophers
import qf4scala._
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.event.Logging
import scala.concurrent.duration._
import scala.collection.mutable.IndexedSeq

abstract class TableEvent(val philosopherId : Int) extends QEvent

case class Done(override val philosopherId : Int) extends TableEvent(philosopherId)
case class Eat(override val philosopherId : Int) extends TableEvent(philosopherId)
case class Hungry(override val philosopherId : Int) extends TableEvent(philosopherId)

class Table(val numberOfPhilosophers : Int, private val system : ActorSystem, private val eventBus : QFEventBus) extends QHsm with Actor {
	private val log = Logging(context.system, this)
	private val forkIsUsed = Array.fill(numberOfPhilosophers)(false)
	private val philosopherIsHungry = Array.fill(numberOfPhilosophers)(false)
	initHsm()
	
	override def initializeStateMachine() = {
		// Subscribe for the relevant events raised by philosophers
		eventBus.subscribe(self, Hungry(0).getClass.getName)

		initializeState(Serving) // initial transition
	}
	
	def receive = {
		case x : QEvent => dispatch(x)
	}

	object Serving extends QState(TopState) {
		override def onEvent(qEvent: QEvent) : Option[QState] = {
			qEvent match {
				case Hungry(philosopherId) => {
					assert (!philosopherIsHungry(philosopherId), "Philosopher must not already be hungry.")

					log.info("Philosopher %d is hungry.".format(philosopherId))
					if (forksFree(philosopherId))
					{
						letPhilosopherEat(philosopherId)
					}
					else
					{
						// The philosopher has to wait for free forks
						philosopherIsHungry(philosopherId) = true // mark philosopher as hungry
						log.info("Philosopher %d has to wait for forks.".format(philosopherId))
					}
				}
				case Done(philosopherId) => {
					log.info("Philosopher %d is done eating.".format(philosopherId))
					philosopherIsHungry(philosopherId) = false

					// free up the philosopher's forks
					freeForks(philosopherId)

					// Can the left philosopher eat?
					var neighborIndex = leftIndex(philosopherId);
					if (philosopherIsHungry(neighborIndex) && forksFree(neighborIndex)) {
						letPhilosopherEat(neighborIndex)
						// The left philosopher could eat; mark philosopher as no longer hungry
						philosopherIsHungry(neighborIndex) = false
					}

					// Can the right philosopher eat?
					neighborIndex = rightIndex(philosopherId);
					if (philosopherIsHungry(neighborIndex) && forksFree(neighborIndex)) {
						letPhilosopherEat(neighborIndex)
						// The right philosopher could eat; mark philosopher as no longer hungry
						philosopherIsHungry(neighborIndex) = false
					}
				}
				case _ => return Some(superState)
			}
			None
		}
	}

	def letPhilosopherEat(philosopherId : Int) = {
		useForks(philosopherId)
		log.info("Table publishes Event event for Philosopher %d.".format(philosopherId))

		eventBus.publish(Eat(philosopherId))
		log.info("Philosopher %d is eating.".format(philosopherId))
	}

	def leftIndex(index : Int) = (index + 1) % numberOfPhilosophers 
	def rightIndex(index : Int) = (index - 1) % numberOfPhilosophers 

	def forksFree(philosopherId : Int) = { !forkIsUsed(philosopherId) && !forkIsUsed(leftIndex(philosopherId)) }

	def useForks(philosopherId : Int) = {
		forkIsUsed(philosopherId) = true
		forkIsUsed(leftIndex(philosopherId)) = true
	}

	def freeForks(philosopherId : Int) = {
		forkIsUsed(philosopherId) = false
		forkIsUsed(leftIndex(philosopherId)) = false
	}
}
