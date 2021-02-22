package org.combinators.ep.language.java     /*DI:LD:AI*/

import org.combinators.ep.generator.NameProvider
import Syntax.MangledName
import com.github.javaparser.StaticJavaParser

import scala.util.Try

/** Provides name mangling for Java */
object JavaNameProvider extends NameProvider[MangledName] {
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
      Try(StaticJavaParser.parseSimpleName(name).getIdentifier).getOrElse {
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
