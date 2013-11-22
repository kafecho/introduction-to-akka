package org.kafecho.learning.spray

import java.util.UUID
import spray.routing._
import akka.actor.{Props, Actor, ActorSystem}
import scala.xml._
import spray.httpx.unmarshalling.Unmarshaller
import spray.httpx.marshalling.Marshaller
import spray.http._
import shapeless.HNil
import shapeless.{:: => :&:}
import scala.xml.NamespaceBinding
import spray.http.HttpHeaders.RawHeader
import akka.io.IO
import spray.can.Http
import org.kafecho.learning.spray.Readers._
import spray.httpx.unmarshalling.Deserializer
import spray.httpx.unmarshalling.MalformedContent


case class Namespace(prefix:String, uri: String)

object XMLProcessor{
  def toScope(namespaces: List[Namespace]) : NamespaceBinding = {
    val topScope : NamespaceBinding = TopScope
    namespaces.foldLeft(topScope)((scope, ns) => NamespaceBinding( ns.prefix, ns.uri, scope))
  }
}

object Constants{
  val xsi = Namespace("xsi","http://www.w3.org/2001/XMLSchema-instance")
  val bms = Namespace("bms","http://base.fims.tv")
  val desc = Namespace("desc","http://description.fims.tv")
  val xml = Namespace("xml","http://www.w3.org/XML/1998/namespace")
  val mainScope = XMLProcessor.toScope(List(xsi, bms, desc, xml))
}

sealed abstract class ErrorCode(val description: String, val statusCode: Option[Int]){
  def statusDescription = for (c <- statusCode; sc <- StatusCodes.getForKey(c)) yield sc.reason
}

object ErrorCode{
  val errorCodes = List(SVC_S00_0019)
  def fromString(s : String) : ErrorCode = errorCodes.find( _.toString == s).get
}

case object SVC_S00_0019 extends ErrorCode("Version mismatch", Some(412) )

//case object SVC_S00_0019 extends ErrorCodeType {
//  override val description = "Version mismatch."
//  override val statusCode = Some(412)
//  override val statusDescription = Some("Precondition failed.")
//  override val introducedInVersion = Some("1_1_0")


case class Fault(code: ErrorCode, detail: Option[String])

case class Description(resourceID: UUID, extensionGroup: List[Elem])
case class BMContentCreateOrUpdate(resourceID: UUID, descriptions: List[Description])
case class BMContent(resourceID: UUID)

sealed trait BMContentsElement
case class BMContentSummary(bmContent: BMContent) extends BMContentsElement
case class BMContentFull(bmContent: BMContent) extends BMContentsElement
case class BMContentLink(bmContent: BMContent) extends BMContentsElement

sealed trait Detail
case object Full extends Detail
case object Summary extends Detail
case object Link extends Detail

case class BMContents(totalSize: Int, detail : Detail, elements: List[BMContentsElement])

case class JobCreate(resourceID: UUID)
case class Job(resourceID: UUID)
sealed trait StartProcess
sealed trait StopProcess

sealed trait StartProcessType

object StartProcessType extends Enumeration{
  type StartProcessType = Value
  val StartProcessByNoWaitType, StartProcessByTimeType, StartProcessByTimeMarkType, StartProcessByServiceDefinedType = Value
}

case object StartProcessByNoWait extends StartProcess
case object StartProcessByTime extends StartProcess
case object StopProcessByTime extends StopProcess

sealed trait JobCommand
case object Stop extends JobCommand
case object Cancel extends JobCommand

case class ManageJobRequest(command: JobCommand)

import Writers._

object Protocols{

  //implicit def genericMarshaller[T](implicit writer: Writer[T]) : Marshaller[T] = Marshaller.delegate[T, NodeSeq](MediaTypes.`application/xml`){ QXML.write[T](_)}
  
  implicit val faultMarshaller : Marshaller[Fault]= Marshaller.delegate[Fault,NodeSeq](MediaTypes.`application/xml`){ QXML.write[Fault](_) }
  
  implicit val faultUnmarshaller : Unmarshaller[Fault] = Unmarshaller.delegate[NodeSeq, Fault](MediaTypes.`application/xml`){ QXML.read[Fault](_) }
  
  
  implicit val bmContentOrCreateUnmarshaller : Unmarshaller[BMContentCreateOrUpdate] = Unmarshaller.delegate[NodeSeq,BMContentCreateOrUpdate](MediaTypes.`application/xml`){nodeSeq : NodeSeq=>
    val resourceID = (nodeSeq \ "resourceID").text
    BMContentCreateOrUpdate(UUID.fromString(resourceID), Nil)
  }

  implicit val bmContentMarshaller : Marshaller[BMContent]= Marshaller.delegate[BMContent,NodeSeq](MediaTypes.`application/xml`){ bmContent : BMContent =>
    val xml = <bms:bmContent><bms:resourceID>urn:uuid:{bmContent.resourceID}</bms:resourceID></bms:bmContent>
    xml.copy(scope = Constants.mainScope)
  }

  implicit val bmContentsMarshaller : Marshaller[BMContents]= Marshaller.delegate[BMContents,NodeSeq](MediaTypes.`application/xml`){ bmContents : BMContents =>
    val xml = <bms:bmContents totalSize={bmContents.totalSize.toString} detail={bmContents.detail.toString}></bms:bmContents>
    xml.copy(scope = Constants.mainScope)
  }

  implicit val jobCreateUnmarshaller : Unmarshaller[JobCreate] = Unmarshaller.delegate[NodeSeq,JobCreate](MediaTypes.`application/xml`){nodeSeq : NodeSeq=>
    val resourceID = (nodeSeq \ "resourceID").text
    JobCreate(UUID.fromString(resourceID))
  }

  implicit val startProcessUnmarshaller = Unmarshaller.delegate[NodeSeq,StartProcess](MediaTypes.`application/xml`){nodeSeq : NodeSeq=>
    StartProcessByTime
  }

  implicit val stopProcessUnmarshaller = Unmarshaller.delegate[NodeSeq,StopProcess](MediaTypes.`application/xml`){nodeSeq : NodeSeq=>
    StopProcessByTime
  }

  implicit val jobMarshaller : Marshaller[Job]= Marshaller.delegate[Job,NodeSeq](MediaTypes.`application/xml`){ job : Job =>
    val xml = <bms:job><bms:resourceID>{job.resourceID}</bms:resourceID></bms:job>
    xml.copy(scope = Constants.mainScope)
  }

  implicit val manageJobRequestUnmarshaller = Unmarshaller.delegate[NodeSeq,ManageJobRequest](MediaTypes.`application/xml`){nodeSeq : NodeSeq=>
    ManageJobRequest(Stop)
  }
}

class InvalidUrnEncodedUuidException extends Exception

object PathMatchers{
  val UrnEncodedUuidMatcher = PathMatcher("""urn:uuid:([\da-fA-F]{8}-[\da-fA-F]{4}-[\da-fA-F]{4}-[\da-fA-F]{4}-[\da-fA-F]{12})""".r)
    .flatMap {
    case string :&: HNil => {
      try Some(UUID.fromString(string) :: HNil)
      catch { case _: IllegalArgumentException => None }
    }
  }
}


object Converters{
  implicit val String2DetailConverter = new Deserializer[String, Detail] {
    def apply(value: String) = value.toLowerCase match {
      case "full" => Right(Full)
      case "summay"=> Right(Summary)
      case "link" => Right(Link)
      case x => Left(MalformedContent("'" + x + "' is not a valid detail value"))
    }
  }
}

trait FimsService extends HttpService{
  //implicit val system = ActorSystem("Test")

  import Protocols._
  import Converters._
  import PathMatchers._

  val contentApi = pathPrefix("content"){
    path(""){
      post{
        entity(as[BMContentCreateOrUpdate]){content =>
          complete(BMContent(UUID.randomUUID))
        }
      } ~
        parameters("skip".as[Int].?, "limit".as[Int].?, "detail".as[Detail].?){ (skip,limit, detail) =>
          get{
            complete(BMContents(5, Full, Nil))
          }
        }
    } ~
      path("purge"){
        post{
          complete(StatusCodes.NoContent)
        } ~
          complete(StatusCodes.MethodNotAllowed, "Method not allowed on purge URL.")
      } ~
      path( UrnEncodedUuidMatcher ){ resourceId : UUID=>
        get{
          complete(BMContent(UUID.randomUUID))
        } ~
          post{
            entity(as[BMContentCreateOrUpdate]){content =>
              complete(BMContent(UUID.randomUUID))
            }
          } ~
          delete{
            complete(StatusCodes.NoContent)
          }
      }
  }

  val jobApi = pathPrefix("job"){path(""){
    post{
      complete("Create a new job")
    } ~
      parameters("skip".as[Int].?, "limit".as[Int].?, "detail".as[Detail].?){ (skip,limit, detail) =>
        get{
          complete(s"Listing all jobs with skip $skip, limit $limit and detail $detail")
        }
      }
  } ~
    path("purge"){
      post{
        complete(StatusCodes.NoContent)
      } ~
        complete(StatusCodes.MethodNotAllowed, "Method not allowed")
    } ~
    pathPrefix( UrnEncodedUuidMatcher) {resourceId : UUID =>
      path(""){
        get{
          complete(Job(UUID.randomUUID))
        } ~
          delete{
            complete(StatusCodes.NoContent)
          }
      } ~
        path("startProcess"){
          entity(as[StartProcess]){startProcess =>
            post{
              complete(StatusCodes.NoContent)
            }
          }
        } ~
        path("stopProcess"){
          post{
            complete(StatusCodes.NoContent)
          }
        } ~
        path("manage"){
          post{
            entity(as[ManageJobRequest]){manageJobRequest =>
              complete(StatusCodes.Accepted)
            }
          }
        }
    }}


  val fimsVersionHeader = "X-Fims-Version"
  val fimsVersion = "1.1"

  /** A custom handler to handle Spray rejection.
    * For example, if a call is rejected because of an invalid Fims version header, returns an appropriate Http message instead of the built-in one.
    */
  implicit val rejectionHandler = RejectionHandler{
    case MalformedHeaderRejection(`fimsVersionHeader`,msg, _) :: _ =>
      completeWithFault(Fault(SVC_S00_0019,Some(msg)))

    case MissingHeaderRejection(`fimsVersionHeader`) :: _ =>
      completeWithFault(Fault(SVC_S00_0019, Some(s"Please specify a valid $fimsVersionHeader header.")))
  }

  /** A directive that checks that the Fims version header is present. If not the request is rejected.*/
  val withFimsVersionHeader : Directive[ HNil] = optionalHeaderValueByName(fimsVersionHeader).flatMap{ header =>
    header match{
      case Some(`fimsVersion`) => pass
      case Some(v) => reject(MalformedHeaderRejection(fimsVersionHeader,s"Unknown version $v", None))
      case None => reject(MissingHeaderRejection(fimsVersionHeader))
    }
  }

  val apiRoute = pathPrefix("api"){
    respondWithHeader(RawHeader(fimsVersionHeader,fimsVersion)){
      handleRejections(rejectionHandler){
        withFimsVersionHeader{ contentApi ~ jobApi }
      }
    }
  }

  /**
   * Formats a Fims Fault as an Http response.
   * @param fault
   * @return
   */
  def completeWithFault(fault: Fault) : Route = {
    val statusCode = (for (key <- fault.code.statusCode; sc <- StatusCodes.getForKey(key)) yield sc).getOrElse(StatusCodes.InternalServerError)
      complete(statusCode, fault)
  }
}

class FimsServiceActor extends Actor with FimsService{
  def actorRefFactory = context
  def receive = runRoute(apiRoute)
}


object FimsServiceActorTest extends App{
 implicit val system = ActorSystem("Test")
 val service = system.actorOf(Props[FimsServiceActor],"fims-service")
 IO(Http) ! Http.Bind(service, "localhost", 8182)
}




