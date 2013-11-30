package org.kafecho.learning.akka

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.ActorRef

class HelloWorld extends Actor{
  // This is the actor's reactive loop.
  def receive : Receive = {
    case "Hello" => println ("World")
    case _ => println ("No comprendo")
  }
}

object HelloWorldTest extends App{
  val system : ActorSystem = ActorSystem("test")
  val ref: ActorRef = system.actorOf(Props[HelloWorld])
  ref ! "Hello"
  ref ! "Bonjour"
}