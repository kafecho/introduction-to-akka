package org.kafecho.learning.akka.remote

import akka.actor.ActorSystem
import akka.actor.Props
import com.typesafe.config.ConfigFactory

object DeployPingTarget extends App{
  val system = ActorSystem("host1", ConfigFactory.load("application1.conf"))
  val actor = system.actorOf(Props[PingTarget], name="pingTarget")
}