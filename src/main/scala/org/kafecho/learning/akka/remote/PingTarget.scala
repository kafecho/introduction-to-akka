package org.kafecho.learning.akka.remote

import akka.actor.Actor

case object Ping
case object Pong

class PingTarget extends Actor{
  def receive = {
    case Ping =>
      println ("Got a ping, replying with a pong....")
      sender ! Pong
  }
}