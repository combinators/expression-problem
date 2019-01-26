package example.expression.scala   /*DI:LD:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.generator.{LanguageIndependentGenerator, Producer}

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

  override def contextDispatch(source:Context, delta:Delta) : Expression = {
    if (delta.expr.isEmpty) {
      throw new scala.NotImplementedError(s""" Self case must be handled by subclass generator. """)
    } else {
      if (delta.op.isDefined) {
        dispatch(delta.expr.get, delta.op.get, delta.params: _*)
      } else {
        dispatch(delta.expr.get, source.op.get, delta.params: _*)
      }
    }
  }

  /**
    * For producer operations, there is a need to instantiate objects, and one would use this
    * method (with specific parameters) to carry this out.
    */
  def inst(exp:domain.Atomic, params:InstanceExpression*): InstanceExpression = {
    Scala("new " + exp.name.capitalize + "(" + params.map(expr => expr.toString).mkString(",") + ")").expression
  }

  /// Scala support

  /** Concatenate attributes by name in order */
  def standardArgs(exp:domain.Atomic) : String = {
    exp.attributes.map(att => att.name + ":" + typeConverter(att.tpe)).mkString(",")
  }

  /** Concatenate attributes by name in order */
  def standardValArgs(exp:domain.Atomic) : String = {
    exp.attributes.map(att => "val " + att.name + ":" + typeConverter(att.tpe)).mkString(",")
  }

  /**
    * Concatenate attributes by name in order, each with a trailing "_" as suffix. These are
    * useful for the parameter to a constructor
    */
  def constructorArgs(exp:domain.Atomic) : String = {
    exp.attributes.map(att => "val " + att.name + "_ :" + typeConverter(att.tpe)).mkString(",")
  }

  /** Concatenate attributes by name in order with comma. */
  def standardParams(exp:domain.Atomic, suffix:String = "") : String = {
    exp.attributes.map(att => att.name + suffix).mkString(",")
  }

  // TODO: CLEANUP
  /**
    * Given (essentially) a method declaration, add statements to occur before existing statements.
    *
    * NOTE: This is a HACK and needs to be fixed
    * @param declaration
    * @param stmts
    * @return
    */
  def addStatements(declaration:Stat, stmts:Seq[Statement]) : Stat = {
    val old:Seq[String] = declaration.toString.split("\n")
    val str = s"def test() : Unit = {\n" + stmts.mkString("\n") + old.tail.tail.mkString("\n")
    Scala(str).declaration()
  }

}
