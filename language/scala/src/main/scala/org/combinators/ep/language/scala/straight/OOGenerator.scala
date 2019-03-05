package org.combinators.ep.language.scala.straight   /*DI:LD:AD*/

import java.nio.file.Paths

import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import org.combinators.ep.language.scala._
import scala.meta.Stat

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait OOGenerator extends ScalaGenerator with ScalaBinaryMethod {

  val domain:BaseDomain with ModelDomain
  import domain._

  def getModel:domain.Model

  /**
    * Generating a straight OO solution requires:
    * 1. A Class for every exp data type
    * 2. A Base class to be superclass of them all
    */
  def generatedCode():Seq[ScalaWithPath] = {
    val flat = getModel.flatten()

    //  binary methods for helper
    val decls:Seq[ScalaWithPath] = if (flat.hasBinaryMethod()) {
      helperClasses()
    } else {
      Seq.empty
    }

    decls ++ flat.types.map(exp => generateExp(exp, flat.ops)) :+      // one trait for each extensions
      generateBase(flat)                                               // base class $BASE
  }

  /** For straight design solution, directly access attributes by name. */
  override def expression (exp:DataType, att:Attribute) : Expression = {
    Scala(s"${att.instance}").expression
  }

  /** Handle self-case here. */
  override def contextDispatch(source:Context, delta:Delta) : Expression = {
    if (delta.expr.isEmpty) {
      val op = delta.op.get.instance
      Scala(s"this.$op${delta.params.mkString("(", ",", ")")}").expression
    } else {
      super.contextDispatch(source, delta)
    }
  }

  /** Directly access local method, one per operation, with a parameter. */
  override def dispatch(expr:Expression, op:Operation, params:Expression*) : Expression = {
    Scala(s"$expr.${op.instance}${params.mkString("(", ",", ")")}").expression
  }

  /** Computer return type for given operation (or void). */
  def returnType(op:Operation): Type = {
    op.returnType match {
      case Some(tpe) => typeConverter(tpe)
      case _ => Scala("Unit").tpe
    }
  }

  /** Operations are implemented as methods in the Base and sub-type classes. */
  def methodGenerator(exp:DataType, op:Operation): Stat = {
    val params = op.parameters.map(param => param.name + ":" + typeConverter(param.tpe)).mkString(",")
    Scala(s"""|
              |def ${op.instance}($params) : ${returnType(op)} = {
              |  ${logic(exp, op).mkString("\n")}
              |}""".stripMargin).statement
  }

  /** Generate the full class for the given expression sub-type. */
  def generateExp(exp:DataType, ops:Seq[Operation]) : ScalaWithPath = {

      val methods = ops.map(op => methodGenerator(exp, op))
      val params = exp.attributes.map(att => s"${att.instance}_ : ${typeConverter(att.tpe)}").mkString(",")
      val locals = exp.attributes.map(att => s"val ${att.instance} = ${att.instance}_")

      ScalaMainWithPath(
        Scala(s"""
                 |package scala_oo
                 |class ${exp.toString}($params) extends ${domain.baseTypeRep.name} {
                 |    ${locals.mkString("\n")}
                 |    ${methods.mkString("\n")}
                 |  }
         """.stripMargin).source(), Paths.get(s"${exp.toString}.scala"))
  }

  /** Generate the base class, with all operations from flattened history. */
  def generateBase(flat:Model): CompilationUnit = {

    val ops = flat.ops.map(op => {
      val pars = op.parameters.map(param => { s"${param.name} : ${typeConverter(param.tpe)}" }).mkString(",")
      s"def ${op.instance}($pars) : ${typeConverter(op.returnType.get)}"
    })

    ScalaMainWithPath(
      Scala(s"""
               |package scala_oo
               |trait ${domain.baseTypeRep.concept} {
               |   ${ops.mkString("\n")}
               |}""".stripMargin).source(), Paths.get(s"${domain.baseTypeRep.concept}.scala"))
  }
}
