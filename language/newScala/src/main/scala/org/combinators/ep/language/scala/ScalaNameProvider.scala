package org.combinators.ep.language.scala    /*DI:LD:AI*/

import com.github.javaparser.{JavaParser, StaticJavaParser}
import org.combinators.ep.generator.NameProvider
import org.combinators.ep.language.inbetween.any

import scala.util.Try

/** Provides name mangling for Java */
class ScalaNameProvider[FT <: FinalTypes](factory: Factory[FT]) extends NameProvider[any.Name[FT]] {
  val parser = new JavaParser(StaticJavaParser.getConfiguration)

  /** Need to have single-param version so this can be used in map. */
  def mangle(name: String): any.Name[FT] = {
    mangle(name, Set("Object", "hashCode", "equals", "toString", "getClass", "type"))
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
  def mangle(name: String, forbidden:Set[String]): any.Name[FT] = {
    var cleanName = name

    // some default methods in java.lang.Object CANNOT be overridden as needed by some AIPs, so
    // take steps to avoid special java methods. To ensure 'equals' and other FFI-required names
    // go through unchanged, we allow for optional parameter to eliminate.
    while (forbidden.contains(cleanName)) {
      cleanName = "_" + cleanName
    }

    factory.name(name, Try(parser.parseSimpleName(cleanName).getResult.map[String](_.getIdentifier).get).getOrElse {
        cleanName.getBytes(java.nio.charset.StandardCharsets.UTF_8).mkString("_", "_", "")
      }
    )
  }

  def addPrefix(prefix: String, name: any.Name[FT]): any.Name[FT] = {
    mangle(prefix + factory.convert(name).component)
  }

  def addSuffix(name: any.Name[FT], suffix: String): any.Name[FT] = {
    mangle(factory.convert(name).component + suffix)
  }
}
