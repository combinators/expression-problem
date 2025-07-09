package org.combinators.ep.language.java     /*DI:LD:AI*/

import org.combinators.cogen.NameProvider
import Syntax.MangledName
import com.github.javaparser.{JavaParser, StaticJavaParser}

import scala.util.Try

/** Provides name mangling for Java */
object JavaNameProvider extends NameProvider[MangledName] {
  val parser = new JavaParser(StaticJavaParser.getParserConfiguration)

  /** Need to have single-param version so this can be used in map. */
  def mangle(name: String): MangledName = {
    mangle(name, Set("Object", "hashCode", "equals", "toString", "getClass"))
  }

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
  def mangle(name: String, forbidden:Set[String]): MangledName = {
    var cleanName = name

    // some default methods in java.lang.Object CANNOT be overridden as needed by some AIPs, so
    // take steps to avoid special java methods. To ensure 'equals' and other FFI-required names
    // go through unchanged, we allow for optional parameter to eliminate.
    while (forbidden.contains(cleanName)) {
      cleanName = "_" + cleanName
    }

    MangledName(cleanName,
      Try(parser.parseSimpleName(cleanName).getResult.map[String](_.getIdentifier).get).getOrElse {
        cleanName.getBytes(java.nio.charset.StandardCharsets.UTF_8).mkString("_", "_", "")
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
