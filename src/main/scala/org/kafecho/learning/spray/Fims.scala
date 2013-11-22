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
import org.joda.time.DateTime
import Writers._
import scala.util.Failure
import akka.actor.Status
import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout


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

case class Description(resourceID: UUID, extensionGroup: NodeSeq)

case class BMContentCreateOrUpdate(resourceID: UUID, descriptions: List[Description])

case class BMContent(resourceID: UUID, revisionID: Int, location: String, resourceCreationDate: DateTime, resourceModifiedDate : DateTime, descriptions: List[Description])

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

case class Located[T](location: String, value : T)

case object StartProcessByNoWait extends StartProcess
case object StartProcessByTime extends StartProcess
case object StopProcessByTime extends StopProcess

sealed trait JobCommand
case object Stop extends JobCommand
case object Cancel extends JobCommand

case class ManageJobRequest(command: JobCommand)

trait Protocols{

  import spray.httpx.marshalling.MetaMarshallers
  
  //implicit def genericMarshaller[T](implicit writer: Writer[T]) : Marshaller[Located[T]] = Marshaller.delegate[Located[T], NodeSeq](MediaTypes.`application/xml`){ l => QXML.write[T](l.value)}
  
  //implicit def genericUnmarshaller[T](implicit r : Reader[T]) : Unmarshaller[T] = Unmarshaller.delegate[NodeSeq, T](MediaTypes.`application/xml`){ QXML.read[T](_) }
  
  //implicit def genericMarshaller[T](implicit writer: Writer[T]) : Marshaller[T] = Marshaller.delegate[T, NodeSeq](MediaTypes.`application/xml`){ QXML.write[T](_)}
  
  implicit val faultMarshaller : Marshaller[Fault]= Marshaller.delegate[Fault,NodeSeq](MediaTypes.`application/xml`){ QXML.write[Fault](_) }
  implicit val faultUnmarshaller : Unmarshaller[Fault] = Unmarshaller.delegate[NodeSeq, Fault](MediaTypes.`application/xml`){ QXML.read[Fault](_) }

  implicit val bmContentMarshaller : Marshaller[BMContent]= Marshaller.delegate[BMContent,NodeSeq](MediaTypes.`application/xml`){ QXML.write(_) }
  implicit val bmContentUnmarshaller : Unmarshaller[BMContent] = Unmarshaller.delegate[NodeSeq, BMContent](MediaTypes.`application/xml`){ QXML.read[BMContent](_) }
  
  implicit val bmContentCreateOrUpdateMarshaller = Marshaller.delegate[BMContentCreateOrUpdate,NodeSeq](MediaTypes.`application/xml`){ QXML.write(_) }
  implicit val bmContentCreateOrUpdateUnmarshaller : Unmarshaller[BMContentCreateOrUpdate] = Unmarshaller.delegate[NodeSeq,BMContentCreateOrUpdate](MediaTypes.`application/xml`){ QXML.read[BMContentCreateOrUpdate](_) }
  
  implicit val bmContentsMarshaller : Marshaller[BMContents]= Marshaller.delegate[BMContents,NodeSeq](MediaTypes.`application/xml`){ QXML.write(_) }

  implicit val jobCreateUnmarshaller : Unmarshaller[JobCreate] = Unmarshaller.delegate[NodeSeq,JobCreate](MediaTypes.`application/xml`){QXML.read[JobCreate](_)}
  
  implicit val JobMarshaller = Marshaller.delegate[Job,NodeSeq](MediaTypes.`application/xml`){ QXML.write(_) }
  
  implicit val stopProcessUnmarshaller = Unmarshaller.delegate[NodeSeq,StopProcess](MediaTypes.`application/xml`) {QXML.read[StopProcess](_)}

  implicit val startProcessUnmarshaller = Unmarshaller.delegate[NodeSeq, StartProcess](MediaTypes.`application/xml`){ QXML.read[StartProcess](_) }
  
  implicit val manageJobRequestUnmarshaller =  Unmarshaller.delegate[NodeSeq, ManageJobRequest](MediaTypes.`application/xml`){ QXML.read[ManageJobRequest](_) }
}

object Protocols extends Protocols

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

case class CreateBMContent(value: BMContentCreateOrUpdate)

class ItemAlreadyExistsException(msg: String) extends Exception

class ContentService(endPoint: String) extends Actor{

  import scala.collection.mutable.Map
  
  val contents : Map[UUID, BMContent] = Map()
  
  def receive : Receive = {
	  case CreateBMContent(value) => 
	    contents.get(value.resourceID) match{
	      case Some(content) => sender ! Status.Failure(new ItemAlreadyExistsException(s"BMContent with identifier urn:uuid:${value.resourceID} already exists."))
	      case None =>
	        val now = DateTime.now()
	        sender ! BMContent(value.resourceID, 0, s"$endPoint/api/content/urn:uuid:${value.resourceID}", now, now, value.descriptions)
	       
	    }
  }
}

trait FimsService extends HttpService{
  //implicit val system = ActorSystem("Test")

  import spray.httpx.marshalling.MetaMarshallers.futureMarshaller
  
  import Converters._
  import PathMatchers._

  def contentService: ActorRef

  //def contentService = context.actorOf(Props(new ContentService("http://localhost:9000/api")))

  
  import scala.concurrent.duration._
  
  //the following line was missing
  implicit def executionContext = actorRefFactory.dispatcher
  
    import Protocols._

  
  implicit val timeout = Timeout(5 seconds)
  
  
  val contentApi = pathPrefix("content"){
    path(""){
      post{
        entity(as[BMContentCreateOrUpdate]){content =>
          val f = (contentService ? CreateBMContent(content)).mapTo[BMContent]
          complete(StatusCodes.Created, f)
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
          complete(BMContent(UUID.randomUUID, 1, "http://localhost:9000/api/content", DateTime.now, DateTime.now, Nil))
        } ~
          post{
            entity(as[BMContentCreateOrUpdate]){content =>
            complete(BMContent(UUID.randomUUID, 1, "http://localhost:9000/api/content", DateTime.now, DateTime.now, content.descriptions))
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

class FimsServiceActor(val contentService: ActorRef) extends Actor with FimsService{
  def actorRefFactory = context
  def receive = runRoute(apiRoute)
}


object FimsServiceActorTest extends App{
 implicit val system = ActorSystem("Test")
 def contentService = system.actorOf(Props(new ContentService("http://localhost:9000/api")))
 val service = system.actorOf(Props(new FimsServiceActor(contentService)),"fims-service")
 IO(Http) ! Http.Bind(service, "localhost", 8182)
}




