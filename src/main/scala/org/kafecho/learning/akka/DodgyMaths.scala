package org.kafecho.learning.akka

import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorSystem
import akka.actor.ActorRef
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy._
import scala.concurrent.duration._

case class Sqrt(d: Double)
case class Result(d: Double)

import scala.concurrent.duration._

class SqrtActor extends Actor {
  var nbRequests = 0
  def receive = {
    case Sqrt(d) =>
      println(s"Computing square root of $d")
      nbRequests += 1
      if (nbRequests % 3 == 0) throw new ArithmeticException("Sorry, I can't do it.")
      Result(Math.sqrt(d))
  }
}

class DodgyMaths extends Actor {
  
  val sqrtActor = context.actorOf(Props[SqrtActor])
  
  // Specifies how the parent handles failure of one of its children.
  override val supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1 minute) {
    case _: ArithmeticException => Stop
  }

  def receive = {
    case msg: Sqrt => sqrtActor forward msg
  }
}

object DodgyMathsTest extends App {
  val system = ActorSystem("Test")
  val maths = system.actorOf(Props[DodgyMaths])
  maths ! Sqrt(24)
  maths ! Sqrt(9)
  maths ! Sqrt(10)
  maths ! Sqrt(13)
  maths ! Sqrt(11)
}