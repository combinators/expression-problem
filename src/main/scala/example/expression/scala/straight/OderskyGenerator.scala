package example.expression.scala.straight

/*DI:LD:AD*/

import java.nio.file.Paths

import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.scala.{Scala, ScalaGenerator, ScalaWithPath}

import scala.meta.Stat

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait OderskyGenerator extends ScalaGenerator {

  val domain:BaseDomain with ModelDomain
  import domain._

  def getModel:domain.Model

  /**
    * Generating a straight OO solution requires:
    * 1. A Class for every exp data type
    * 2. A Base class to be superclass of them all
    */
  def generatedCode():Seq[ScalaWithPath] = {
    getModel.inChronologicalOrder.map(m => generateExp(m)) :+      // one trait for each extensions
      generateBase()                                               // base class $BASE

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

  /** Computer return type for given operation (or void). */
  def returnType(op:Operation): Type = {
    op.returnType match {
      case Some(tpe) => typeConverter(tpe)
      case _ => Scala("Unit").tpe()
    }
  }

  /** Operations are implemented as methods in the Base and sub-type classes. */
  def methodGenerator(exp:Atomic)(op:Operation): Stat = {
    val params = "" // parameters(op)
    val str:String = s"""|
             |def ${op.name}($params) : ${returnType(op)} = {
                         |  ${logic(exp)(op).mkString("\n")}
                         |}""".stripMargin
    println ("mg:" + str)
    Scala(str).statement()

  }

  /** Generate the full class for the given expression sub-type. */
  def generateExp(model:Model) : CompilationUnit = {
    val mcaps = model.name.capitalize

    val classes:Seq[scala.meta.Stat] = model.types.map(exp => {
      val methods = model.ops.map(methodGenerator(exp))
      val params = exp.attributes.map(att => s"${att.name}_ : ${typeConverter(att.tpe)}").mkString(",")
      val locals = exp.attributes.map(att => s"val ${att.name} = ${att.name}_")

      val str =
        s"""
           |class ${exp.toString}($params) extends ${domain.baseTypeRep.name} {
           |    ${locals.mkString("\n")}
           |    ${methods.mkString("\n")}
           |  }
         """.stripMargin
      println ("str:" + str)
      Scala(str).declaration()
    })

    val newOps = model.ops.map(op => {
      val pars = op.parameters.map(pair => { s"${pair._1} : ${typeConverter(pair._2)}" }).mkString(",")
      s"def ${op.name}($pars) : ${typeConverter(op.returnType.get)}"
    })

    ScalaWithPath(
      Scala(s"""|package scala_oo
             |trait $mcaps extends Base {
             |  type exp <: Exp
             |
             |  trait Exp extends super.Exp {
             |    ${newOps.mkString("\n")}
             |  }
             |
             |  ${classes.mkString("\n")}
             |}""".stripMargin).source(), Paths.get(s"$mcaps.scala"))
  }

  /** Generate the base class, with all operations from flattened history. */
  def generateBase(): CompilationUnit = {

      val str:String = s"""
                          |trait Base {
                          |   type ${domain.baseTypeRep.name.toLowerCase()} <: ${domain.baseTypeRep.name.capitalize}
                          |   trait ${domain.baseTypeRep.name.capitalize} {
                          |
                          |   }
                          |}""".stripMargin


    ScalaWithPath(
      Scala(str).source(), Paths.get(s"Base.scala"))
  }
}
