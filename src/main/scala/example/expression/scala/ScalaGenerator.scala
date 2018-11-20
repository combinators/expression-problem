package example.expression.scala   /*DI:LD:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.generator.LanguageIndependentGenerator
import scala.meta._

/**
  * Any Scala-based EP approach can extend this Generator
  */
trait ScalaGenerator extends LanguageIndependentGenerator with DependentDispatch {
  val domain:BaseDomain with ModelDomain

  type CompilationUnit = ScalaWithPath
  type Type = scala.meta.Type
  type Expression = scala.meta.Term
  type Statement = scala.meta.Stat

  /** Return designated Java type associated with type, or void if all else fails. */
  override def typeConverter(tpe:domain.TypeRep) : Type = {
    tpe match {
      case domain.baseTypeRep => scala.meta.Type.Name(domain.baseTypeRep.name)
      case _ => super.typeConverter(tpe)
    }
  }

  /**
    * Responsible for delegating to a new operation on the current context.
    */
  def delegate(exp:domain.Atomic, op:domain.Operation, params:Expression*) : Expression = {
    val opargs = params.mkString(",")
    val term = Term.Name(op.name.toLowerCase)   // should be able to be ..$params
    Scala(s"this.${op.name.toLowerCase}($opargs)").expression()
  }
}
