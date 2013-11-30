package org.kafecho.learning.akka
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.Terminated

case object AreYouSarahConnor

class SarahConnor extends Actor{
  def receive = {
    case AreYouSarahConnor =>
      println ("I am Sarah Connor. How may I help you?")
      sender ! true
  }
  
  override def postStop = {
    println ("Nooooooooooooooooo")
  }
  
}

class Terminator(target: ActorRef) extends Actor{
  
  context.watch(target)

  target ! AreYouSarahConnor
  
  def receive = {
    case true if (sender == target) =>
      println ("I am the Terminator, I have been sent from the future to kill you....")
      context.stop(target)
    case Terminated(`target`) =>
      println ("@skynet: Mission accomplished.")
      context.stop(self)
  }
}

object TerminatorTest extends App{
  val system = ActorSystem("Skynet")
  val sarahConnor = system.actorOf(Props[SarahConnor])
  val terminator  = system.actorOf(Props(new Terminator(sarahConnor)))
}