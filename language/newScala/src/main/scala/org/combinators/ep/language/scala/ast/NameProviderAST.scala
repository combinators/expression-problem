package org.combinators.ep.language.scala.ast

import com.github.javaparser.{JavaParser, StaticJavaParser}
import org.combinators.cogen.NameProvider
import org.combinators.ep.language.inbetween.any
import scala.util.Try

trait NameProviderAST { self: BaseAST =>
  object nameProvider {
    trait FinalTypes {
      type NameProvider <: nameProvider.ScalaNameProvider
    }
    
    trait ScalaNameProvider extends NameProvider[any.Name] {
      def getSelfNameProvider: nameProviderFinalTypes.NameProvider
      
      val parser = new JavaParser(StaticJavaParser.getParserConfiguration)

      /** Need to have single-param version so this can be used in map. */
      def mangle(name: String): any.Name = {
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
      def mangle(name: String, forbidden: Set[String]): any.Name = {
        var cleanName = name

        // some default methods in java.lang.Object CANNOT be overridden as needed by some AIPs, so
        // take steps to avoid special java methods. To ensure 'equals' and other FFI-required names
        // go through unchanged, we allow for optional parameter to eliminate.
        while (forbidden.contains(cleanName)) {
          cleanName = "_" + cleanName
        }

        self.scalaBaseFactory.name(name, Try(parser.parseSimpleName(cleanName).getResult.map[String](_.getIdentifier).get).getOrElse {
            cleanName.getBytes(java.nio.charset.StandardCharsets.UTF_8).mkString("_", "_", "")
          }
        )
      }

      def addPrefix(prefix: String, name: any.Name): any.Name = {
        mangle(prefix + factory.convert(name).component)
      }

      def addSuffix(name: any.Name, suffix: String): any.Name = {
        mangle(factory.convert(name).component + suffix)
      }
    }
    
    trait Factory {
      def scalaNameProvider: nameProvider.ScalaNameProvider
      
      implicit def convert(other: nameProvider.ScalaNameProvider): nameProviderFinalTypes.NameProvider =
        other.getSelfNameProvider
    }
  }
  
  val nameProviderFinalTypes: nameProvider.FinalTypes
  val nameProviderFactory: nameProvider.Factory
}

trait FinalNameProviderAST extends NameProviderAST { self: BaseAST =>
  object finalNameProviderFinalTypes {
    trait NameProviderFinalTypes extends nameProvider.FinalTypes {
      type NameProvider = nameProvider.ScalaNameProvider
    }
  }

  object finalNameProviderFactoryTypes {
    trait NameProviderFactory extends nameProvider.Factory {
      def scalaNameProvider: nameProvider.ScalaNameProvider = {
        case class ScalaNameProvider() extends nameProvider.ScalaNameProvider {
          override def getSelfNameProvider: nameProvider.ScalaNameProvider = this
        }
        ScalaNameProvider()
      }
    }
  }

  override val nameProviderFinalTypes: finalNameProviderFinalTypes.NameProviderFinalTypes = new finalNameProviderFinalTypes.NameProviderFinalTypes {}
  override val nameProviderFactory: finalNameProviderFactoryTypes.NameProviderFactory = new finalNameProviderFactoryTypes.NameProviderFactory {}
}