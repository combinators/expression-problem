package example.expression.scala   /*DI:LD:AI*/

import scala.meta._
import org.apache.commons.text.StringEscapeUtils
import play.twirl.api.{BufferedContent, Format, Formats}

import scala.collection.immutable

class Scala private(elements: immutable.Seq[Scala], text: String) extends BufferedContent[Scala](elements, text) {
  def this(text: String) = this(Nil, Formats.safe(text))
  def this(elements: immutable.Seq[Scala]) = this(elements, "")

  private lazy val fullText: String = (text +: elements).mkString

  /** Content type of Scala */
  val contentType = "text/x-scala"

  /** Top level. */
  def source(): Source = fullText.parse[Source].get
  //def pkg() : Pkg = fullText.parse[Pkg].get
  //def obj(): scala.meta.Pkg.Object = fullText.parse[Pkg.Object].get

  /** Parses an import declaration. */
  //def importDeclaration(): Import = fullText.parse[Import].get

  /** Parses this element as declaration */
  def declaration(): Stat = fullText.parse[Stat].get

  def term : Term = fullText.parse[Term].get

  /** We need a seq[stat] because other languages create sequences. */
  def statements : Seq[Stat] =  dialects.Sbt1(fullText).parse[Source].get.stats

  def statement : Stat = fullText.parse[Stat].get

  def expression : Term = fullText.parse[Term].get

  /** Parses this element as a (potentially qualified) name. */
  //def name(): Name = fullText.parse[Name].get

  /** Parses this element as an interface body declaration (e.g. a method signature). */
  def definition: Stat = fullText.parse[Stat].get

  /** Parses this element as a type (e.g. the in  X foo = (X)bar). */
  def tpe: Type = fullText.parse[Type].get
}

/** Helper for Scala utility methods. */
object Scala {
  /** Creates a Scala fragment with initial content specified. */
  def apply(text: String): Scala = new Scala(text)

  /** Creates a Scala fragment with initial content from the given `text` separated by `separator`. */
  def apply(text: Seq[String], separator: String = ";"): Scala = {
    apply(text.mkString(separator))
  }
}

object ScalaFormat extends Format[Scala] {
  /** Integrates `text` without performing any escaping process.
    *
    * @param text Text to integrate
    */
  def raw(text: String): Scala = Scala(text)

  /** Escapes `text` using Scala String rules.
    *
    * @param text Text to integrate
    */
  def escape(text: String): Scala = new Scala(StringEscapeUtils.escapeJava(text))

  /** Generates an empty Scala fragment */
  val empty: Scala = new Scala("")

  /** Creates an Scala Fragment that holds other fragments. */
  def fill(elements: immutable.Seq[Scala]): Scala = new Scala(elements)

}
