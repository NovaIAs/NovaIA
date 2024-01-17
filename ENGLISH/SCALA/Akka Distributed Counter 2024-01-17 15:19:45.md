```
import akka.actor.{ ActorRef, ActorSystem, Props }
import akka.util.Timeout

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

object DistributedCounter extends App {
  implicit val system = ActorSystem()
  implicit val ec = system.dispatcher
  implicit val timeout = Timeout(5 seconds)

  val actorRef1: ActorRef = system.actorOf(Props[CounterActor], "CounterActor1")
  val actorRef2: ActorRef = system.actorOf(Props[CounterActor], "CounterActor2")

  Await.result(actorRef1 ? Increment, timeout.duration)
  Await.result(actorRef2 ? Increment, timeout.duration)

  val result1: Int = Await.result(actorRef1 ? GetCount, timeout.duration)
  val result2: Int = Await.result(actorRef2 ? GetCount, timeout.duration)

  println(s"Actor 1: $result1, Actor 2: $result2")

  actorRef1 ! PoisonPill
  actorRef2 ! PoisonPill

  system.terminate()
}

class CounterActor extends Actor {
  var count: Int = 0

  def receive = {
    case Increment => count += 1
    case GetCount => sender() ! count
  }
}

case object Increment
case object GetCount
case object PoisonPill
```

Explanation:

- The code creates two actors, `CounterActor1` and `CounterActor2`, which both have a variable `count` that stores the current count.
- The main function sends an `Increment` message to both actors, incrementing their counts.
- The main function then sends a `GetCount` message to both actors, which return their current counts.
- The main function prints the results to the console.
- The main function then sends a `PoisonPill` message to both actors, which tells them to terminate.
- The main function waits for the actors to terminate and then shuts down the actor system.

This code demonstrates how to use actors in Akka to create distributed counters. Actors are a powerful tool for concurrency and distribution in Scala.