package org.combinators.ep.language.gj     /*DI:LD:AI*/

import org.apache.commons.text.StringEscapeUtils
import play.twirl.api.{BufferedContent, Format, Formats}

import scala.collection.immutable

/**
  * http://homepages.inf.ed.ac.uk/wadler/topics/gj.html
  *
  * Encoding of Wadler's original GJ Expression Problem solution
  *
  * @param tpe
  */
class GJType (val tpe:String) {
  override def toString = tpe
}

class GJ private(elements: immutable.Seq[GJ], text: String) extends BufferedContent[GJ](elements, text) {
  def this(text: String) = this(Nil, Formats.safe(text))
  def this(elements: immutable.Seq[GJ]) = this(elements, "")

  private lazy val fullText: String = (text +: elements).mkString

  /** Content type of GJ */
  val contentType = "text/x-gj"

  /** Indents this fragment by 4 spaces. */
  def indent: GJ = {
    GJ(fullText.lines.map(l => s"    $l").mkString("\n"))
  }

  /** Indents everything except the first line in this fragment by 4 spaces. */
  def indentExceptFirst: GJ = {
    val lines: Seq[String] = fullText.lines.toSeq
    GJ((lines.head +: lines.tail.map(l => s"    $l")).mkString("\n"))
  }

  /** Returns the code of this fragment as a String. */
  def getCode: String = fullText
}

class GJStatement protected (elements: immutable.Seq[GJStatement], text: String) extends BufferedContent[GJStatement](elements, text) {
  def this(text: String) = this(Nil, Formats.safe(text))
  def this(elements: immutable.Seq[GJStatement]) = this(elements, "")

  private lazy val fullText: String = (text +: elements).mkString

  /** Content type of Haskell */
  val contentType = "text/x-haskell"

  /** Indents this fragment by 4 spaces. */
  def indent: GJ = {
    GJ(fullText.lines.map(l => s"    $l").mkString("\n"))
  }

  /** Indents everything except the first line in this fragment by 4 spaces. */
  def indentExceptFirst: GJ = {
    val lines: Seq[String] = fullText.lines.toSeq
    GJ((lines.head +: lines.tail.map(l => s"    $l")).mkString("\n"))
  }

  /** Returns the code of this fragment as a String. */
  def getCode: String = fullText
}

object GJStatement {
  /** Creates a Haskell fragment with initial content specified. */
  def apply(text: String): GJStatement = {
    new GJStatement(text)
  }
}

/**
  * Helper for GJ utility methods.
  */
object GJ {
  /** Creates a GJ fragment with initial content specified. */
  def apply(text: String): GJ = {
    new GJ(text)
  }

  /** Creates a GJ fragment with initial content from the given `text` separated by `separator`. */
  def apply(text: Seq[String], separator: String = ";"): GJ = {
    apply(text.mkString(separator))
  }
}

object GJFormat extends Format[GJ] {
  /**
    * Integrates `text` without performing any escaping process.
    *
    * @param text Text to integrate
    */
  def raw(text: String): GJ = GJ(text)

  /**
    * Escapes `text` using GJ String rules.
    *
    * @param text Text to integrate
    */
  def escape(text: String): GJ = GJ(StringEscapeUtils.escapeJava(text))

  /** Generates an empty GJ fragment */
  val empty: GJ = new GJ("")

  /** Creates a GJ fragment that holds other fragments. */
  def fill(elements: immutable.Seq[GJ]): GJ = new GJ(elements)

}

