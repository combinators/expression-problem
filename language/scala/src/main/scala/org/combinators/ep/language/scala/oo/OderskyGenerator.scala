package org.combinators.ep.language.scala.oo   /*DI:LD:AD*/

import java.nio.file.Paths

import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import org.combinators.ep.language.scala._

import scala.meta.Stat

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait OderskyGenerator extends ScalaGenerator with ScalaBinaryMethod {

  val domain:BaseDomain with ModelDomain
  import domain._

  def getModel:domain.Model

  /**
    * Generating an OO solution (ala Odersky 2004) requires:
    * 1. A Class for every exp data type
    * 2. A Base class to be superclass of them all
    */
  def generatedCode():Seq[ScalaWithPath] = {

    //  binary methods for helper
    val decls:Seq[ScalaWithPath] = if (getModel.flatten().hasBinaryMethod) {
      helperClasses()
    } else {
      Seq.empty
    }

    decls ++ getModel.inChronologicalOrder.tail.map(m => generateExp(m)) :+      // one trait for each extensions
      generateBase(getModel.base())                                // base class

  }

  /** For straight design solution, directly access attributes by name. */
  override def expression (exp:DataType, att:Attribute) : Expression = {
    Scala(s"${att.instance}").expression
  }

  /** Directly access local method, one per operation, with a parameter. */
  override def dispatch(expr:Expression, op:Operation, params:Expression*) : Expression = {
    val args:String = params.mkString(",")
    Scala(s"$expr.${op.name}($args)").expression
  }

  /** Handle self-case here. */
  override def contextDispatch(source:Context, delta:Delta) : Expression = {
    if (delta.expr.isEmpty) {
      val op = delta.op.get.instance
      val args:String = delta.params.mkString(",")
      Scala(s"this.$op($args)").expression
    } else {
      super.contextDispatch(source, delta)
    }
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
    val params = op.parameters.map(param => s"${param.name} : ${typeConverter(param.tpe)}").mkString(",")
    val str:String = s"""|
             |def ${op.name}($params) : ${returnType(op)} = {
                         |  ${logic(exp, op).mkString("\n")}
                         |}""".stripMargin
    Scala(str).statement
  }

  /** Generate the full class for the given expression sub-type. */
  def generateExp(model:Model) : CompilationUnit = {
    val mcaps = model.name.capitalize
    val prior = model.last.name.capitalize

    val classes:Seq[scala.meta.Stat] = model.types.map(exp => {
      val methods = model.pastOperations().map(op => methodGenerator(exp, op))
      val params = exp.attributes.map(att => s"${att.instance}_ : ${typeConverter(att.tpe)}").mkString(",")
      val locals = exp.attributes.map(att => s"val ${att.instance} = ${att.instance}_")

      val str =
        s"""
           |class ${exp.concept}($params) extends ${domain.baseTypeRep.concept} {
           |    ${locals.mkString("\n")}
           |    ${methods.mkString("\n")}
           |  }
         """.stripMargin
      Scala(str).declaration()
    })

    val preamble = if (model.ops.isEmpty) {
      Seq.empty
    } else {
      val newOps = model.ops.map(op => {
        val pars = op.parameters.map(param => s"${param.name} : ${typeConverter(param.tpe)}").mkString(",")
        s"def ${op.name}($pars) : ${typeConverter(op.returnType.get)}"
      })

      val narrow =
        s"""
           |type ${domain.baseTypeRep.instance} <: ${domain.baseTypeRep.concept}
           |trait ${domain.baseTypeRep.concept} extends super.${domain.baseTypeRep.concept} {
           |  ${newOps.mkString("\n")}
           |}
         """.stripMargin

      Scala(narrow).statements ++
      model.pastDataTypes().map(exp => {
        val methods = model.ops.map(op => methodGenerator(exp, op))
        val params = exp.attributes.map(att => s"${att.instance}_ : ${typeConverter(att.tpe)}").mkString(",")
        val args = exp.attributes.map(att => att.instance + "_").mkString(",")
        val locals = exp.attributes.map(att => s"override val ${att.instance} = ${att.instance}_")

        val str =
          s"""
             |class ${exp.concept}($params) extends super.${exp.concept}($args) with ${domain.baseTypeRep.concept} {
             |    ${locals.mkString("\n")}
             |    ${methods.mkString("\n")}
             |  }
           """.stripMargin
        Scala(str).declaration()
      })
    }

    ScalaMainWithPath(
      Scala(s"""|package odersky
             |trait $mcaps extends $prior {
             |  ${classes.mkString("\n")}
             |  ${preamble.mkString("\n")}
             |}""".stripMargin).source(), Paths.get(s"$mcaps.scala"))
  }

  /** Generate the base class, with all operations from flattened history. */
  def generateBase(baseModel:Model): CompilationUnit = {

    val initialOpsDef = baseModel.ops.map(op =>
      s"def ${op.instance}() : ${typeConverter(op.returnType.get)}")

    // hack: initial op has no parameters...
    val initialTypes = baseModel.types.map(exp => {
      val initialOpsLogic = baseModel.ops.map(op => {
        s"def ${op.instance}() = " + logic(exp, op).mkString("\n")
      })

      // yes, scala requires a space between _ and :
      val atts = exp.attributes.map(att => s"val ${att.instance} = ${att.instance}_")
        val str = s"""
                     |class ${exp.concept}(${constructorArgs(exp)}) extends ${domain.baseTypeRep.concept} {
                     |  ${atts.mkString("\n")}
                     |  ${initialOpsLogic.mkString("\n")}
                     |}""".stripMargin
        Scala(str).declaration()
        })

      val str:String = s"""package odersky
                          |trait ${baseModel.name.capitalize} {
                          |   type ${domain.baseTypeRep.instance} <: ${domain.baseTypeRep.concept}
                          |   trait ${domain.baseTypeRep.concept} {
                          |     ${initialOpsDef.mkString("\n")}
                          |   }
                          |
                          |   ${initialTypes.mkString("\n")}
                          |}""".stripMargin

    ScalaMainWithPath(
      Scala(str).source(), Paths.get(s"${baseModel.name.capitalize}.scala"))
  }
}
