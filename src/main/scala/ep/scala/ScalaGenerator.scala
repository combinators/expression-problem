package ep.scala   /*DI:LD:AI*/

import ep.domain.{BaseDomain, ModelDomain}
import ep.generator.{LanguageIndependentGenerator, Producer}

import scala.meta._

/**
  * Any Scala-based EP approach can extend this Generator
  */
trait ScalaGenerator extends LanguageIndependentGenerator with Producer {
  val domain:BaseDomain with ModelDomain

  type CompilationUnit = ScalaWithPath
  type Type = scala.meta.Type
  type Expression = scala.meta.Term
  type Statement = scala.meta.Stat
  type InstanceExpression = scala.meta.Term

  /** Return designated Java type associated with type, or void if all else fails. */
  override def typeConverter(tpe:domain.TypeRep) : Type = {
    tpe match {
      case domain.baseTypeRep => scala.meta.Type.Name(domain.baseTypeRep.name)
      case _ => super.typeConverter(tpe)
    }
  }

  /**
    * Default behavior in Scala is to return an expression value as is
    */
  def result (expr:Expression) : Seq[Statement] = {
    Seq(expr)
  }

  /**
    * For producer operations, there is a need to instantiate objects, and one would use this
    * method (with specific parameters) to carry this out.
    */
  def inst(exp:domain.Atomic, params:InstanceExpression*): InstanceExpression = {
    Scala("new " + exp.concept + "(" + params.map(expr => expr.toString).mkString(",") + ")").expression
  }

  /// Scala support

  /** Concatenate attributes by name in order */
  def standardArgs(exp:domain.Atomic) : String = {
    exp.attributes.map(att => att.instance + ":" + typeConverter(att.tpe)).mkString(",")
  }

  /** Concatenate attributes by name in order */
  def standardValArgs(exp:domain.Atomic) : String = {
    exp.attributes.map(att => "val " + att.instance + ":" + typeConverter(att.tpe)).mkString(",")
  }

  /**
    * Concatenate attributes by name in order, each with a trailing "_" as suffix. These are
    * useful for the parameter to a constructor
    */
  def constructorArgs(exp:domain.Atomic) : String = {
    exp.attributes.map(att => "val " + att.instance + "_ :" + typeConverter(att.tpe)).mkString(",")
  }

  /** Concatenate attributes by name in order with comma. */
  def standardParams(exp:domain.Atomic, suffix:String = "") : String = {
    exp.attributes.map(att => att.instance + suffix).mkString(",")
  }
}
