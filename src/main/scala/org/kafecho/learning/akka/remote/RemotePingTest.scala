package org.kafecho.learning.akka.remote

import akka.actor.ActorSystem
import akka.actor.Props
import com.typesafe.config.ConfigFactory

object RemotePingTest extends App{
  val system = ActorSystem("host2", ConfigFactory.load("application2.conf"))
  val target = system.actorFor("akka://host1@127.0.0.1:2001/user/pingTarget")
  val pingActor = system.actorOf(Props(new PingActor(target)))
  pingActor ! Ping
}