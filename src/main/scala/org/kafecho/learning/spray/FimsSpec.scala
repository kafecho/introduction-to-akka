package org.kafecho.learning.spray

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import Protocols._
import spray.testkit.ScalatestRouteTest
import spray.http.StatusCodes
import akka.actor.Props
import java.util.UUID
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

/**
 * @author: Guillaume Belrose
 */
@RunWith(classOf[JUnitRunner])
class FimsSpec extends WordSpec with ShouldMatchers with ScalatestRouteTest with FimsService {
  def actorRefFactory = system // connect the DSL to the test ActorSystem

  val contentService = system.actorOf(Props(new ContentService("http://localhost:9000/api")))

  "The Fims service" should {

    "reject requests which do not contain a X-Fims-Version header with an Http 412 error" in {
      Get("/api/content") ~> apiRoute ~> check {
        status === StatusCodes.ExpectationFailed
        val fault = entityAs[Fault]
        fault.code === SVC_S00_0019
      }
    }

    "reject requests which contain a X-Fims-Version header with an incorrect version with an Http 412 error" in {
      Get("/api/content") ~> addHeader("X-Fims-Version", "3.13") ~> apiRoute ~> check {
        status == StatusCodes.ExpectationFailed
        val fault = entityAs[Fault]
        fault.code === SVC_S00_0019
      }
    }

    "accept requests to /api/content when provided with a valid X-Fims-Version header" in {
      Get("/api/content") ~> addHeader("X-Fims-Version", "1.1") ~> apiRoute ~> check {
        status === StatusCodes.OK
      }
    }

//    "create a new BMContent object and return HTTP 204 with an BMContent object which contains a location link (which can be de-referenced)" in {
//      val toCreate = BMContentCreateOrUpdate(UUID.randomUUID, Nil)
//      Post("/api/content", toCreate) ~> apiRoute ~> check {
//        status == StatusCodes.Accepted
//        //          val created = entityAs[BMContent]
//        //          header("Content-Location") match{
//        //            case None => fail("Missing location header")
//        //            case Some(location) =>
//        //              location === created.location
//        //          }
//      }
//    }
  }
}