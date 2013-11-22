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

trait Reader[T]{
  def read(input: NodeSeq) : T
}

trait Writer[T]{
  def write(value: T) : NodeSeq
}

object QXML{
  def read[T](input: NodeSeq)(implicit reader: Reader[T]) : T = reader.read(input)
  def write[T](value: T)(implicit writer: Writer[T]) : NodeSeq = writer.write(value)
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

object Writers{
  implicit object FaultWriter extends Writer[Fault]{
    def write(fault: Fault) = {
      val xml = <fault><code>{fault.code.toString}</code><description>{fault.code.description}</description>{fault.detail.map(d => <detail>{d}</detail>).getOrElse(Null)}</fault>
      QXmlProcessor.setPrefixIfAbsent(xml,Constants.bms.prefix).copy(scope = Constants.mainScope)
    }
  }
}

object Readers{

  implicit object UuidReader extends Reader[UUID]{
    def read(input: NodeSeq) = {
      UUID.fromString(input.text.toLowerCase.stripPrefix("urn:uuid:"))}
  }

  implicit object IntReader extends Reader[Int]{
    def read(input: NodeSeq) = input.text.toInt
  }

  implicit object OptionStringReader extends Reader[Option[String]]{
    def read(input: NodeSeq) = {
      if (input.isEmpty) None
      else Some(input.text)
    }
  }

  implicit object OptionIntReader extends Reader[Option[Int]]{
    def read(input: NodeSeq) = QXML.read[Option[String]](input).map( _.toInt)
  }


  implicit object StringReader extends Reader[String]{
    def read(input: NodeSeq) = input.text
  }

  implicit object ErrorCodeReader extends Reader[ErrorCode]{
    def read(input: NodeSeq) = ErrorCode.fromString(input.text)
  }

  implicit object FaultReader extends Reader[Fault]{
    def read(input: NodeSeq) = {
      Fault (
        QXML.read[ErrorCode](input \ "code"),
        QXML.read[Option[String]](input \ "detail")
      )
    }
  }

  implicit object DateTimeReader extends Reader[DateTime]{
    def read(input: NodeSeq) = ISODateTimeFormat.dateTime().parseDateTime(QXML.read[String](input))
  }

  implicit object StartProcessByNoWaitReader extends Reader[StartProcessByNoWait.type]{
    def read(input: NodeSeq) = StartProcessByNoWait
  }
}

object QXmlProcessor{
   def setPrefixIfAbsent(elem: Elem, prefix: String): Elem = {
	  setPrefixIfAbsentRecur(elem, prefix)(0).asInstanceOf[Elem]
  }

  
  def setPrefixIfAbsentRecur(input: Seq[Node], prefix: String): Seq[Node] = {
    for (node <- input) yield node match {
      case elem: Elem =>
        val children = elem.child.toSeq
        val newPrefix = Option(elem.prefix).getOrElse(prefix)
        elem.copy(prefix = newPrefix, child = setPrefixIfAbsentRecur(children, prefix))
      case other => other
    }
  }
 
}

object Foo{
  def hi = "hi"
}


object Main extends App{
  
  import Readers._
  val xml = <time>2013-03-08T21:43:20.546Z</time>
  val dateTime = QXML.read[DateTime](xml)
  println (dateTime)
  
  val scope1 = Constants.mainScope
  val scope2 = NamespaceBinding( "bar", "http://www.bar.com", scope1)
  val xml2 = (<xml2>Bar<sub>Hello world</sub></xml2>).copy(scope = scope2)
  val xml1 = (<xml1>{xml2}</xml1>).copy(scope = scope1)
  
 
}
