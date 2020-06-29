package org.combinators.ep.language.scala   /*DI:LD:AI*/

import org.combinators.ep.generator.NameProvider
import Syntax.MangledName
import scala.meta._

import scala.util.Try


/** Provides name mangling for Scala */
object ScalaNameProvider extends NameProvider[MangledName] {
  /** Tries to parse names as a
    * [[https://www.javadoc.io/static/org.scalameta/trees_2.12/4.3.7/scala/meta/Term$$Name.html name]] and mangles to
    * the arabic number representation of the UTF-8 bytes in the given string, where each byte is prefixed by "_".
    *
    * Example:
    * {{
    * ScalaNameProvider.mangle("foo") // returns "foo"
    * ScalaProvider.mangle("if") // returns "_105_102" because "if" is a reserved keyword
    * }}
    */
  def mangle(name: String): MangledName = {
    MangledName(name,
      Try(name.parse[Term].get.asInstanceOf[Term.Name].value).getOrElse {
        name.getBytes(java.nio.charset.StandardCharsets.UTF_8).mkString("_", "_", "_")
      }
    )
  }

  def addPrefix(prefix: String, name: MangledName): MangledName = {
    mangle(prefix + name.original)
  }

  def addSuffix(name: MangledName, suffix: String): MangledName = {
    mangle(name.original + suffix)
  }
}
