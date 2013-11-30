package org.kafecho.learning.akka

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props

case object Irritate
case object CalmDown
case object WhoAmI

class Hulk extends Actor{

  def BruceBanner : Receive = {
    case WhoAmI => println ("My name is Bruce Banner, I am a nuclear physicist")
    case CalmDown => println ("I am calm")
    case Irritate => 
      println ("Oh no, stay away from me, I am becoming Hulk....")
      context become Hulk
  }

  def Hulk : Receive = {
    case WhoAmI=> println ("Me se gonna smash your face")
    case Irritate => println ("Me cannot get any greener")
    case CalmDown =>
      println ("Me se getting less green")
      context become BruceBanner
  }

  
  // Initial state, start as Bruce Banner
  def receive = BruceBanner
}

object HulkTest extends App{
  val system = ActorSystem("Marvel")
  val hulk = system.actorOf(Props[Hulk])
  
  hulk ! WhoAmI
  hulk ! CalmDown
  hulk ! Irritate
  hulk ! WhoAmI
  hulk ! Irritate
  hulk ! CalmDown
  hulk ! CalmDown

}