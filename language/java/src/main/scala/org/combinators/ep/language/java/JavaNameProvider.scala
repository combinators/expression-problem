package org.combinators.ep.language.java

import org.combinators.ep.generator.{FreshNameProvider, NameProvider}
import org.combinators.templating.twirl.Java
import Syntax.MangledName
import Syntax.default.Name

import scala.util.Try
import cats.data._
import com.github.javaparser.ast.expr.SimpleName




/** Provides name mangling for Java */
object MangledNameProvider extends NameProvider[MangledName] {
  /** Tries to parse names as a
    * [[https://docs.oracle.com/javase/specs/jls/se7/html/jls-6.html#jls-6.2 simple Java name]] and mangles to
    * the arabic number representation of the UTF-8 bytes in the given string, where each byte is prefixed by "_".
    *
    * Example:
    * {{
    * JavaNameProvider.mangle("foo") // returns "foo"
    * JavaNameProvider.mangle("class") // returns "_99_108_97_115_115" because "class" is a reserved keyword
    * }}
    */
  def mangle(name: String): MangledName = {
    MangledName(name,
      Try(Java(name).simpleName().asString).getOrElse {
      name.getBytes(java.nio.charset.StandardCharsets.UTF_8).mkString("_", "_", "")
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

object JavaNameProvider extends NameProvider[Name] {
  def mangle(name: String): Name = State.pure(MangledNameProvider.mangle(name))

  def addPrefix(prefix: String, name: Name): Name =
    name.map(MangledNameProvider.addPrefix(prefix, _))

  def addSuffix(name: Name, suffix: String): Name =
    name.map(MangledNameProvider.addSuffix(_, suffix))
}
