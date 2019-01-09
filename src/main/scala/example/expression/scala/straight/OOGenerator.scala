package example.expression.scala.straight   /*DI:LD:AD*/

import java.nio.file.Paths

import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.scala._

import scala.meta.{Source, Stat, Term}

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait OOGenerator extends ScalaGenerator with ScalaBinaryMethod with StandardScalaBinaryMethod {

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
    val decls:Seq[ScalaWithPath] = if (flat.ops.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      helperClasses()
    } else {
      Seq.empty
    }

    decls ++ flat.types.map(exp => generateExp(exp, flat.ops)) :+      // one trait for each extensions
      generateBase(flat)                                               // base class $BASE
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
    * Responsible for delegating to a new operation on the current data-type, identified by exp.
    *
    * In this straight-oo solution, 'this' is always the context and the 'exp' parameter
    * can be ignored.
    */
  override def delegateFixMe(exp:domain.Atomic, op:domain.Operation, params:Expression*) : Expression = {
    val opargs = params.mkString(",")
    val term = Term.Name(op.name.toLowerCase)   // should be able to be ..$params
    Scala(s"this.${op.name.toLowerCase}()").expression()
  }

  /** For Scala generator, same behavior as delegate. */
  override def identify(exp:domain.Atomic, op:domain.Operation, params:Expression*) : Expression = {
    delegateFixMe(exp, op, params : _*)
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
    val params = op.parameters.map(pair => pair._1 + ":" + typeConverter(pair._2)).mkString(",")

    val str:String = s"""|
             |def ${op.name}($params) : ${returnType(op)} = {
                         |  ${logic(exp)(op).mkString("\n")}
                         |}""".stripMargin
    Scala(str).statement()
  }

  /** Generate the full class for the given expression sub-type. */
  def generateExp(exp:Atomic, ops:Seq[Operation]) : ScalaWithPath = {

      val methods = ops.map(methodGenerator(exp))
      val params = exp.attributes.map(att => s"${att.name}_ : ${typeConverter(att.tpe)}").mkString(",")
      val locals = exp.attributes.map(att => s"val ${att.name} = ${att.name}_")

      val str =
        s"""
           |package scala_oo
           |class ${exp.toString}($params) extends ${domain.baseTypeRep.name} {
           |    ${locals.mkString("\n")}
           |    ${methods.mkString("\n")}
           |  }
         """.stripMargin

      ScalaMainWithPath(
        Scala(str).source(), Paths.get(s"${exp.toString}.scala"))
  }

  /** Generate the base class, with all operations from flattened history. */
  def generateBase(flat:Model): CompilationUnit = {

    val ops = flat.ops.map(op => {
      val pars = op.parameters.map(pair => { s"${pair._1} : ${typeConverter(pair._2)}" }).mkString(",")
      s"def ${op.name}($pars) : ${typeConverter(op.returnType.get)}"
    })

    val str:String = s"""
                  |package scala_oo
                  |trait ${domain.baseTypeRep.name.capitalize} {
                  |   ${ops.mkString("\n")}
                  |}""".stripMargin

    ScalaMainWithPath(
      Scala(str).source(), Paths.get(s"${domain.baseTypeRep.name.capitalize}.scala"))
  }
}
