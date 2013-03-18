package diningPhilosophers

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.event.Logging
import scala.concurrent.duration._

object Main extends App {
	val system = ActorSystem("DiningPhilosophersSystem")
	val eventBus = new QFEventBus
  
	//Use the system's dispatcher as ExecutionContext
    import system.dispatcher
    
    val numberOfPhilosophers = 5
    val table = system.actorOf(Props(new Table(numberOfPhilosophers, system, eventBus)), name = "Table")
	val philosophers = new Array[akka.actor.ActorRef](numberOfPhilosophers)
	
	for (index <- 0 to philosophers.length - 1) {
		philosophers(index) = system.actorOf(Props(new Philosopher(index, system, eventBus)), name = "Philosopher" + index)
	}
	
	//system.scheduler.scheduleOnce(2 seconds, philosophers(0), Timeout())

	println("%d philosophers gather around a table thinking ....".format(numberOfPhilosophers))
//	table.initHsm()
//	for (index <- 0 to philosophers.length - 1) {
//		philosophers(index).initHsm()
//	}
}