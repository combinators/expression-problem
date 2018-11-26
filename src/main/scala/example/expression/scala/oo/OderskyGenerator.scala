package example.expression.scala.oo    /*DI:LD:AD*/

import java.nio.file.Paths

import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.scala._

import scala.meta.{Stat, Term}

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait OderskyGenerator extends ScalaGenerator with ScalaBinaryMethod with StandardScalaBinaryMethod {

  val domain:BaseDomain with ModelDomain
  import domain._

  def getModel:domain.Model

  /**
    * Generating an OO solution (ala Odersky 2004) requires:
    * 1. A Class for every exp data type
    * 2. A Base class to be superclass of them all
    */
  def generatedCode():Seq[ScalaWithPath] = {
    getModel.inChronologicalOrder.tail.map(m => generateExp(m)) :+      // one trait for each extensions
      generateBase(getModel.base())                                // base class $BASE

  }

  /** For straight design solution, directly access attributes by name. */
  override def subExpressions(exp:Atomic) : Map[String,Expression] = {
    exp.attributes.map(att => att.name -> Scala(s"${att.name}").expression()).toMap
  }

  /** Directly access local method, one per operation, with a parameter. */
  override def dispatch(expr:Expression, op:Operation, params:Expression*) : Expression = {
    val args:String = params.mkString(",")
    Scala(s"$expr.${op.name}($args)").expression()
  }

  /**
    * Responsible for delegating to a new operation on the current context.
    */
  override def delegate(exp:domain.Atomic, op:domain.Operation, params:Expression*) : Expression = {
    val opargs = params.mkString(",")
    val term = Term.Name(op.name.toLowerCase)   // should be able to be ..$params
    Scala(s"new ${exp.name.capitalize}($opargs).${op.name.toLowerCase()}()").expression()
  }

  /** Computer return type for given operation (or void). */
  def returnType(op:Operation): Type = {
    op.returnType match {
      case Some(tpe) => typeConverter(tpe)
      case _ => Scala("Unit").tpe()
    }
  }

  /** Operations are implemented as methods in the Base and sub-type classes. */
  def methodGenerator(exp:Atomic)(op:Operation): Stat = {
    val params = op.parameters.map(pair => {
      s"${pair._1} : ${typeConverter(pair._2)}"
    }).mkString(",")
    val str:String = s"""|
             |def ${op.name}($params) : ${returnType(op)} = {
                         |  ${logic(exp)(op).mkString("\n")}
                         |}""".stripMargin
    Scala(str).statement()
  }

  /** Generate the full class for the given expression sub-type. */
  def generateExp(model:Model) : CompilationUnit = {
    val mcaps = model.name.capitalize
    val prior = if (model.last.equals(model.base())) {
      "Base"
    } else {
      model.last.name.capitalize
    }

    val classes:Seq[scala.meta.Stat] = model.types.map(exp => {
      val methods = model.pastOperations().map(methodGenerator(exp))
      val params = exp.attributes.map(att => s"${att.name}_ : ${typeConverter(att.tpe)}").mkString(",")
      val locals = exp.attributes.map(att => s"val ${att.name} = ${att.name}_")

      val str =
        s"""
           |class ${exp.toString}($params) extends ${domain.baseTypeRep.name} {
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
        val pars = op.parameters.map(pair => {
          s"${pair._1} : ${typeConverter(pair._2)}"
        }).mkString(",")
        s"def ${op.name}($pars) : ${typeConverter(op.returnType.get)}"
      })

      val narrow =
        s"""
           |type ${domain.baseTypeRep.name.toLowerCase()} <: ${domain.baseTypeRep.name.capitalize}
           |trait ${domain.baseTypeRep.name.capitalize} extends super.${domain.baseTypeRep.name.capitalize} {
           |  ${newOps.mkString("\n")}
           |}
         """.stripMargin

      Scala(narrow).statements() ++
      model.pastDataTypes().map(exp => {
        val methods = model.ops.map(methodGenerator(exp))
        val params = exp.attributes.map(att => s"${att.name}_ : ${typeConverter(att.tpe)}").mkString(",")
        val args = exp.attributes.map(att => att.name + "_").mkString(",")
        val locals = exp.attributes.map(att => s"override val ${att.name} = ${att.name}_")

        val str =
          s"""
             |class ${exp.toString}($params) extends super.${exp.toString}($args) with ${domain.baseTypeRep.name} {
             |    ${locals.mkString("\n")}
             |    ${methods.mkString("\n")}
             |  }
           """.stripMargin
        Scala(str).declaration()
      })
    }

    ScalaWithPath(
      Scala(s"""|package scala_oo
             |trait $mcaps extends $prior {
             |  ${classes.mkString("\n")}
             |  ${preamble.mkString("\n")}
             |}""".stripMargin).source(), Paths.get(s"$mcaps.scala"))
  }

  /** Generate the base class, with all operations from flattened history. */
  def generateBase(base:Model): CompilationUnit = {

    val initialOpsDef = base.ops.map(op =>
      s"def ${op.name.toLowerCase()}() : ${typeConverter(op.returnType.get)}")

    // hack: initial op has no parameters...
    val initialTypes = base.types.map(exp => {
      val initialOpsLogic = base.ops.map(op => {
        s"def ${op.name.toLowerCase()}() = " + logic(exp)(op).mkString("\n")
      })

      // yes, scala requires a space between _ and :
      val stdArgs = exp.attributes.map(att => "val " + att.name + "_ :" + typeConverter(att.tpe)).mkString(",")

      val atts = exp.attributes.map(att => s"val ${att.name.toLowerCase()} = ${att.name.toLowerCase()}_")
        val str = s"""
                     |class ${exp.name.capitalize}($stdArgs) extends ${domain.baseTypeRep.name} {
                     |  ${atts.mkString("\n")}
                     |  ${initialOpsLogic.mkString("\n")}
                     |}""".stripMargin
        Scala(str).declaration()
        })

      val str:String = s"""
                          |trait Base {
                          |   type ${domain.baseTypeRep.name.toLowerCase()} <: ${domain.baseTypeRep.name.capitalize}
                          |   trait ${domain.baseTypeRep.name.capitalize} {
                          |     ${initialOpsDef.mkString("\n")}
                          |   }
                          |
                          |   ${initialTypes.mkString("\n")}
                          |}""".stripMargin


    ScalaWithPath(
      Scala(str).source(), Paths.get(s"Base.scala"))
  }
}
