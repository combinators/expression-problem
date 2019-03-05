package org.combinators.ep.language.haskell.straight  /*DI:LD:AD*/

import java.nio.file.Paths

import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import org.combinators.ep.language.haskell._

// https://eli.thegreenplace.net/2016/the-expression-problem-and-its-solutions/

trait StraightGenerator extends HaskellGenerator with StandardHaskellBinaryMethod with HaskellBinaryMethod {
  val domain:BaseDomain with ModelDomain
  import domain._

  def getModel: domain.Model

  lazy val flat:domain.Model = getModel.flatten()

  /** For the processed model, return generated code artifacts for solution. */
  def generatedCode():Seq[HaskellWithPath] = {
    flat.ops.map(op => generateOp(flat, op)) :+
      generateDataTypes(flat)
  }

  def generateOp(m:Model, op:Operation) : HaskellWithPath = {
    val opRetType = typeConverter(op.returnType.get)
    val extraOp = op.parameters.map(param => {    // what happens when two params?
      val tpe = param.tpe
      if (tpe.equals(domain.baseTypeRep)) {
        s""" -> ${domain.baseTypeRep.name} """
      } else {
        tpe  // not sure what else to do
      }
    }).mkString("")

    val definition = Haskell(s"${op.instance} :: ${domain.baseTypeRep.name} $extraOp -> $opRetType")

    val instances  = {
      val definedInstances = m.types.map(exp => {
        val opsParam = op.parameters.map(param => {    // what happens when two params?
          if (param.tpe.equals(domain.baseTypeRep)) {
            s"""(${exp.concept} ${standardArgs(exp, "2").getCode})"""
          } else {
            param.name  // not sure what else to do
          }
        }).mkString("")

        s"""${op.instance} (${exp.concept} ${standardArgs(exp).getCode}) $opsParam = ${logic(exp, op).mkString("\n")}"""
     })

      // handle default case as needed
      requireDefault(op) match {
        case None => definedInstances
        case Some((numParams,defaultVal)) => definedInstances :+ defaultCase(new Haskell(op.instance), numParams, defaultVal)
      }
    }

    val dependencies = dependency(op).map(op => s"import ${op.concept}").mkString("\n")
    val code = Haskell(s"""|module ${op.concept} where
                           |import DataTypes
                           |
                           |${addedImports(op).mkString("\n")}
                           |$dependencies
                           |$definition
                           |${instances.mkString("\n")}""".stripMargin)
    HaskellWithPath(code, Paths.get(s"${op.concept}.hs"))
  }

    /**
      * Responsible for dispatching sub-expressions with possible parameter(s).
      * Seems safest to include/embed parens here
      */
    override def dispatch(primary:Haskell, op:domain.Operation, params:Haskell*) : Haskell = {
      val args:String = if (params.isEmpty) {
        ""
      } else {
        params.map(h => "(" + h.getCode + ")").mkString(" ")
      }

      Haskell(s"""(${op.instance} (${primary.toString}) $args)""")
    }

  /** For straight design solution, directly access attributes by name. */
  override def expression (exp:DataType, att:Attribute) : Expression = {
    Haskell(s"${att.instance}")
  }

  /**
    * For producer operations, there is a need to instantiate objects, and one would use this
    * method (with specific parameters) to carry this out.
    */
  override def inst(exp:domain.DataType, params:Expression*): CodeBlockWithResultingExpressions = {
    CodeBlockWithResultingExpressions(Haskell(exp.concept + " " + params.map(h => "(" + h.getCode + ")").mkString(" ")))
  }
}