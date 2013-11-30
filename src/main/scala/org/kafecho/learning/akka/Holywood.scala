package org.kafecho.learning.akka

import akka.actor.ActorSystem
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props

case object Action
case object SayLine
case class Line(s: String)

class DeNiro extends Actor {
  def receive = {
    case SayLine =>
      println("Saying my line")
      sender ! Line("Are you talkin' to me?")
  }
}

class Scorcese(maxTries: Int, deNiro: ActorRef) extends Actor {
  var nbTries = 0
  def receive = {
    case Action =>
      println("And action!!!")
      deNiro ! SayLine

    case Line(_) =>
      nbTries += 1
      if (nbTries == maxTries){
        println ("It's a wrap well done everybody!!!")
        context.stop(deNiro)
        context.stop(self)
      }else{
        deNiro ! SayLine
      }
  }
}

object Holywood extends App {
  val system = ActorSystem("Holywood")
  val deNiro = system.actorOf(Props[DeNiro])
  val scorcese = system.actorOf(Props(new Scorcese(10,deNiro)))
  scorcese ! Action
}