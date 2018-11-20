package example.expression.haskell      /*DI:LD:AI*/

import org.apache.commons.text.StringEscapeUtils
import play.twirl.api.{BufferedContent, Format, Formats}

import scala.collection.immutable

class HaskellType (val tpe:String) {
  override def toString = tpe
}

class Haskell private(elements: immutable.Seq[Haskell], text: String) extends BufferedContent[Haskell](elements, text) {
  def this(text: String) = this(Nil, Formats.safe(text))
  def this(elements: immutable.Seq[Haskell]) = this(elements, "")

  private lazy val fullText: String = (text +: elements).mkString

  /** Content type of Haskell */
  val contentType = "text/x-haskell"

  /** Indents this fragment by 4 spaces. */
  def indent: Haskell = {
    Haskell(fullText.lines.map(l => s"    $l").mkString("\n"))
  }

  /** Indents everything except the first line in this fragment by 4 spaces. */
  def indentExceptFirst: Haskell = {
    val lines: Seq[String] = fullText.lines.toSeq
    Haskell((lines.head +: lines.tail.map(l => s"    $l")).mkString("\n"))
  }

  /** Returns the code of this fragment as a String. */
  def getCode: String = fullText
}

/**
  * Helper for Haskell utility methods.
  */
object Haskell {
  /** Creates a Haskell fragment with initial content specified. */
  def apply(text: String): Haskell = {
    new Haskell(text)
  }
}

/**
  * Given an arbitrary number of parameters, turn this into ( a b c )
  */
object HaskellSignature {
  /** Creates a Haskell fragment with initial content specified. */
  def apply(text: String*): Haskell = {
    new Haskell("(" + text.toSeq.mkString(" ") + ")")
  }
}

object HaskellFormat extends Format[Haskell] {
  /**
    * Integrates `text` without performing any escaping process.
    *
    * @param text Text to integrate
    */
  def raw(text: String): Haskell = Haskell(text)

  /**
    * Escapes `text` using Haskell String rules.
    *
    * @param text Text to integrate
    */
  def escape(text: String): Haskell = Haskell(StringEscapeUtils.escapeJava(text))

  /** Generates an empty Haskell fragment */
  val empty: Haskell = new Haskell("")

  /** Creates a Haskell fragment that holds other fragments. */
  def fill(elements: immutable.Seq[Haskell]): Haskell = new Haskell(elements)

}

