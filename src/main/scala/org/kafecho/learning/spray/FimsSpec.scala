package org.kafecho.learning.spray

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import Protocols._
import spray.testkit.ScalatestRouteTest
import spray.http.StatusCodes

/**
 * @author: Guillaume Belrose
 */
class FimsSpec extends WordSpec with ShouldMatchers with ScalatestRouteTest with FimsService {
  def actorRefFactory = system // connect the DSL to the test ActorSystem

  "The Fims service" should {
    "reject requests which do not contain a X-Fims-Version header with an Http 412 error" in {
      Get("/api/content") ~> apiRoute ~> check {
        status == StatusCodes.ExpectationFailed
        val fault = entityAs[Fault]
        fault.code == SVC_S00_0019
      }
    }

    "reject requests which contain a X-Fims-Version header with an incorrect version with an Http 412 error" in {
      Get("/api/content") ~> addHeader("X-Fims-Version", "3.13") ~> apiRoute ~> check {
        status == StatusCodes.ExpectationFailed
        val fault = entityAs[Fault]
        fault.code == SVC_S00_0019
      }
    }

    "accept requests to /api/content when provided with a valid X-Fims-Version header" in {
      Get("/api/content") ~> addHeader("X-Fims-Version", "1.1") ~> apiRoute ~> check {
        status == StatusCodes.OK
      }
    }

  }
}
