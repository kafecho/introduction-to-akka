package org.kafecho.learning.spray

import scala.xml.NodeSeq
import scala.util.Try
import java.util.UUID
import org.kafecho.learning.spray._
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import scala.xml.NamespaceBinding
import scala.xml.Node
import scala.xml.Elem
import scala.xml.Null
import scala.xml.XML

trait Reader[T] {
  def read(input: NodeSeq): T
}

trait Writer[T] {
  def write(value: T): NodeSeq
}

object QXML {
  def read[T](input: NodeSeq)(implicit reader: Reader[T]): T = reader.read(input)
  def write[T](value: T)(implicit writer: Writer[T]): NodeSeq = writer.write(value)
}

//object XmlProcessor{
//  def setPrefixIfAbsent(input: Seq[Node], prefix: String): Seq[Node] = {
//    for (node <- input) yield node match {
//      case elem: Elem =>
//        val children = elem.child.toSeq
//        val newPrefix = Option(elem.prefix).getOrElse(prefix)
//        elem.copy(prefix = newPrefix, child = setPrefixIfAbsent(children, prefix))
//      case other => other
//    }
//  }
//}

object Writers {
  implicit object FaultWriter extends Writer[Fault] {
    def write(fault: Fault) = {
      val xml = <fault><code>{ fault.code.toString }</code><description>{ fault.code.description }</description>{ fault.detail.map(d => <detail>{ d }</detail>).getOrElse(Null) }</fault>
      QXmlProcessor.setNamespaceIfAbsent(xml, Constants.bms, Constants.mainScope)
    }
  }

  implicit object DescriptionWriter extends Writer[Description] {
    def write(value: Description) = {
      val extensionGroup = value.extensionGroup match {
        case NodeSeq.Empty => Null
        case n => <ExtensionGroup>{ n }</ExtensionGroup>
      }
      <description><resourceID>urn:uuid:{ value.resourceID }</resourceID>{ extensionGroup }</description>
    }
  }

  implicit object BMContentCreateOrUpdateWriter extends Writer[BMContentCreateOrUpdate] {
    def write(value: BMContentCreateOrUpdate) = {
      val xml = <bmContent><resourceID>urn:uuid:{ value.resourceID }</resourceID><descriptions>{ value.descriptions.map(QXML.write[Description](_)) }</descriptions></bmContent>
      QXmlProcessor.setNamespaceIfAbsent(xml, Constants.bms, Constants.mainScope)
    }
  }

  implicit object BMContentWriter extends Writer[BMContent] {
    def write(value: BMContent) = {
      val xml =
        <bmContent>
          <resourceID>urn:uuid:{value.resourceID}</resourceID>
          <revisionID>{value.revisionID}</revisionID>
          <descriptions>{value.descriptions.map(QXML.write[Description](_))}</descriptions>
          <resourceCreationDate>{ISODateTimeFormat.dateTime().print(value.resourceCreationDate)}</resourceCreationDate>
          <resourceModifiedDate>{ISODateTimeFormat.dateTime().print(value.resourceCreationDate)}</resourceModifiedDate>
        </bmContent>
      QXmlProcessor.setNamespaceIfAbsent(xml, Constants.bms, Constants.mainScope)
    }
  }
  
  implicit object JobWriter extends Writer[Job]{
    def write(value: Job) = {
      val xml = <job><resourceID>{value.resourceID}</resourceID></job>
      QXmlProcessor.setNamespaceIfAbsent(xml, Constants.bms, Constants.mainScope)
    }
  }
  
  implicit object BMContentsWriter extends Writer[BMContents]{
    def write(value: BMContents) = {
      val xml = <bmContents/>
      QXmlProcessor.setNamespaceIfAbsent(xml, Constants.bms, Constants.mainScope)
    }
  }
  
}

object Readers {
  implicit object UuidReader extends Reader[UUID] {
    def read(input: NodeSeq) = {
      UUID.fromString(input.text.toLowerCase.stripPrefix("urn:uuid:"))
    }
  }

  implicit object DescriptionReader extends Reader[Description] {
    def read(input: NodeSeq) = {
      Description(QXML.read[UUID](input \ "resourceID"), (input \ "ExtensionGroup")(0).child)
    }
  }

  implicit object BMContentCreateOrUpdateReader extends Reader[BMContentCreateOrUpdate] {
    def read(input: NodeSeq) = {
      BMContentCreateOrUpdate(
        QXML.read[UUID](input \ "resourceID"),
        (input \ "descriptions" \ "description").map(node => QXML.read[Description](node)).toList)
    }
  }

  
  implicit object StringReader extends Reader[String] {
    def read(input: NodeSeq) = input.text
  }
  
  implicit object IntReader extends Reader[Int] {
    def read(input: NodeSeq) = input.text.toInt
  }
  
  implicit object DateTimeReader extends Reader[DateTime] {
    def read(input: NodeSeq) = ISODateTimeFormat.dateTime().parseDateTime(QXML.read[String](input))
  }
  
  implicit object BMContentReader extends Reader[BMContent] {
    def read(input: NodeSeq) = {
      BMContent(
        QXML.read[UUID](input \ "resourceID"),
        QXML.read[Int](input \ "revisionID"),
        QXML.read[String](input \ "location"),
        QXML.read[DateTime](input \ "resourceCreationDate"),
        QXML.read[DateTime](input \ "resourceModifiedDate"),
        (input \ "descriptions" \ "description").map(node => QXML.read[Description](node)).toList
      )
    }
  }

  implicit object OptionStringReader extends Reader[Option[String]] {
    def read(input: NodeSeq) = {
      if (input.isEmpty) None
      else Some(input.text)
    }
  }

  implicit object OptionIntReader extends Reader[Option[Int]] {
    def read(input: NodeSeq) = QXML.read[Option[String]](input).map(_.toInt)
  }

  implicit object ErrorCodeReader extends Reader[ErrorCode] {
    def read(input: NodeSeq) = ErrorCode.fromString(input.text)
  }

  implicit object FaultReader extends Reader[Fault] {
    def read(input: NodeSeq) = {
      Fault(
        QXML.read[ErrorCode](input \ "code"),
        QXML.read[Option[String]](input \ "detail"))
    }
  }

  implicit object StartProcessReader extends Reader[StartProcess] {
    def read(input: NodeSeq) = StartProcessByNoWait
  }
  
  implicit object StopProcessReader extends Reader[StopProcess] {
    def read(input: NodeSeq) = StopProcessByTime
  }

  implicit object JobReader extends Reader[Job]{
    def read(input: NodeSeq) = Job(QXML.read[UUID](input \ "resourceID"))
  }
  
  implicit object JobCreateReader extends Reader[JobCreate]{
    def read(input: NodeSeq) = JobCreate(QXML.read[UUID](input \ "resourceID"))
  }

  implicit object ManageJobRequestReader extends Reader[ManageJobRequest]{
    def read(input: NodeSeq) = ManageJobRequest(Stop)
  }

}

object QXmlProcessor {

  def setNamespaceIfAbsent(elem: Elem, namespace: Namespace, scope: NamespaceBinding): Elem = {
    setPrefixIfAbsentRecur(elem, namespace.prefix)(0).asInstanceOf[Elem].copy(scope = scope)
  }

  private def setPrefixIfAbsent(elem: Elem, prefix: String): Elem = {
    setPrefixIfAbsentRecur(elem, prefix)(0).asInstanceOf[Elem]
  }

  private def setPrefixIfAbsentRecur(input: Seq[Node], prefix: String): Seq[Node] = {
    for (node <- input) yield node match {
      case elem: Elem =>
        val children = elem.child.toSeq
        val newPrefix = Option(elem.prefix).getOrElse(prefix)
        elem.copy(prefix = newPrefix, child = setPrefixIfAbsentRecur(children, prefix))
      case other => other
    }
  }

}

object Main extends App {

  import Readers._
  import Writers._

  val xml = <time>2013-03-08T21:43:20.546Z</time>
  val dateTime = QXML.read[DateTime](xml)
  println(dateTime.toString())

  val scope1 = Constants.mainScope
  val scope2 = NamespaceBinding("bar", "http://www.bar.com", scope1)
  val xml2 = (<xml2>Bar<sub>Hello world</sub></xml2>).copy(scope = scope2)
  val xml1 = (<xml1>{ xml2 }</xml1>).copy(scope = scope1)

  val attributes = <isa:Attributes xmlns:isa="http://isa.quantel.com">
                     <isa:Title>Games of the century</isa:Title>
                     <isa:Category>NTSC</isa:Category>
                     <isa:Owner>ESPN History</isa:Owner>
                     <isa:Description>Best games of the past 100 years.</isa:Description>
                   </isa:Attributes>

  val bm = BMContentCreateOrUpdate(UUID.randomUUID(), List(Description(UUID.randomUUID(), attributes)))

  val written = QXML.write(bm).toString

  println(written)

  val parsed = QXML.read[BMContentCreateOrUpdate](XML.loadString(written))

  println("Scope is" + parsed.descriptions(0).extensionGroup)
  
  println (QXML.write(BMContent(UUID.randomUUID, 1, "http://localhost:9000/api/content", DateTime.now, DateTime.now, bm.descriptions)))

  
}
