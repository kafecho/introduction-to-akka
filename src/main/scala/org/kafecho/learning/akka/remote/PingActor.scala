package org.kafecho.learning.akka.remote

import akka.actor.ActorRef
import akka.actor.Actor

class PingActor(target : ActorRef) extends Actor{
 def receive = {
   case Ping =>
     println (s"Sending a ping to $target")
     target ! Ping
   case Pong => println (s"Got a pong back from $sender")
 }
}