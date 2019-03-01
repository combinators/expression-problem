package org.combinators.ep.language.haskell.alacarte  /*DI:LD:AD*/

import java.nio.file.Paths

import org.combinators.ep.language.haskell.{HUnitTestGenerator, Haskell, HaskellType, HaskellWithPath}
import org.combinators.ep.domain.{BaseDomain, ModelDomain}

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait ALaCarteTestGenerator extends HUnitTestGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._

  val flat:domain.Model

//  /** normalize the atomic Instance by its position within flattened data types. */
//  def treeRoute(a:UnaryInst, flattened:Seq[DataType]) : String = {
//    if (flattened.size == 1) {
//      s"${a.e.concept}"
//    } else if (a.e.name == flattened.head.name) {
//      s"El(${a.e.concept} "
//    } else {
//      "Er(" + treeRoute(a, flattened.tail) + " "
//    }
//  }
//
//  def treeRoute(a:BinaryInst, flattened:Seq[DataType]) : String = {
//    if (flattened.size == 1) {
//      s"${a.e.concept}"
//    } else if (a.e.name == flattened.head.name) {
//      s"El(${a.e.concept} "
//    } else {
//      "Er(" + treeRoute(a, flattened.tail) + " "
//    }
//  }
//
//  def treeRoute(a:AtomicInst, flattened:Seq[DataType]) : String = {
//    if (flattened.size == 1) {
//      s"${a.e.concept}"
//    } else if (a.e.name == flattened.head.name) {
//      s"El(${a.e.concept} "
//    } else {
//      "Er(" + treeRoute(a, flattened.tail) + " "
//    }
//  }

  // ugly! works, though...

//  def closeTreeRoute(a:Inst, flattened:Seq[DataType]) : String = {
//    if (flattened.size == 1) {
//      ""
//    } else if (a.name == flattened.head.name) {
//      ")"
//    } else {
//      ")" + closeTreeRoute(a, flattened.tail)
//    }
//  }

  /**
    * Actual value in a test case.
    *
    * Each basic test case has an instance over which an operation is to be performed. This method
    * returns the inline expression resulting from dispatching operation, op, over the given instance, inst.
    *
    */
  override def actual(op: domain.Operation, inst: domain.Inst, params: Expression*): CodeBlockWithResultingExpressions = {
    toTargetLanguage(inst).appendDependent(instExp => {
      val expr:Expression = contextDispatch(NoSource, deltaExprOp(instExp.head, op, params: _*))
      //CodeBlockWithResultingExpressions(Haskell(expr.getCode + ":: GeneralExpr"))
      CodeBlockWithResultingExpressions(Haskell(expr.getCode))
    })
  }

  /**
    * Expand instance into its post-order traversal of interior definitions.
    *
    * a1 = (7*2)
    *
    * a1 = In(Er(Er(BinaryMul In(El(Constant 7.0))  In(El(Constant 2.0)))))
    */
//  override def convert(inst:Inst) : Haskell = {
//    Haskell(convert0(inst).getCode + ":: GeneralExpr")
//  }

//  /** Recursive helper method. Creates the prefix In(Er(El(... Followed by dataType name. */
//  def convert0(inst:Inst) : Haskell = {
//    val name = inst.name
//    inst match {
//      case ui: UnaryInst =>
//        Haskell(s"In(" + treeRoute(ui, flat.types) + s" (${convert0(ui.inner)}) " + closeTreeRoute(inst, flat.types) + ")")
//
//      case bi: BinaryInst =>
//          Haskell(s"In(" + treeRoute(bi, flat.types) + s" (${convert0(bi.left)}) (${convert0(bi.right)}) " + closeTreeRoute(inst, flat.types) + ")")
//
//      case exp: AtomicInst =>
//        Haskell(s"In(" + treeRoute(exp, flat.types) + exp.ei.inst + closeTreeRoute(inst, flat.types) + ")")
//
//      case _ => Haskell(s""" -- unknown $name" """)
//    }
//  }

//  /**
//    * For producer operations, there is a need to instantiate objects, and one would use this
//    * method (with specific parameters) to carry this out.
//    */
//  def inst(exp:domain.DataType, params:Expression*): CodeBlockWithResultingExpressions = {
//    CodeBlockWithResultingExpressions(
//
//
//      Haskell(s"FIXME ${exp.concept}${params.mkString("(", ", ", ")")}")
//    )
//  }

  /**
    * Convert a scala expression into the target language.
    *
    * The ExistsInstance could be a primitive type (Double, String, int) and if a domain instance, the request
    * is delegated to toTargetLanguage(domain.Inst)
    *
    * @param scalaValue   The ExistsInstance captures a type (TypeRep) and an instance which varies based on trait.
    * @return
    */
  override def toTargetLanguage(scalaValue:ExistsInstance) : CodeBlockWithResultingExpressions = {
    scalaValue.inst match {
      case domInst: domain.Inst => toTargetLanguage(domInst)
      case _ => throw new scala.NotImplementedError(s"No rule to convert ${scalaValue.tpe} to the target language")
    }
  }

  /**
    * Convert a domain specific data type instance into the target language.
    *
    * Already configured for all known cases of DomainInst, namely [[UnaryInst]], [[BinaryInst]] and [[AtomicInst]]
    *
    * Need to add parens to parameters
    * @param instance
    * @return
    */
  override def toTargetLanguage(instance: domain.Inst): CodeBlockWithResultingExpressions = {
    instance match {
      case ui: domain.UnaryInst =>
        toTargetLanguage(ui.inner).appendDependent(innerResults => {
          val parens = innerResults.map(exp => Haskell(exp.getCode.mkString("(", "", ")")))
          inst(ui.e, parens:_*)
        })

      case bi: domain.BinaryInst =>
        toTargetLanguage(bi.left)
          .appendIndependent(toTargetLanguage(bi.right))
          .appendDependent(innerResults => {
            val parens = innerResults.map(exp => Haskell(exp.getCode.mkString("(", "", ")")))
            inst(bi.e, parens: _*)
          })

      case ai:domain.AtomicInst => // no need for params since atomic
        toTargetLanguage(ai.ei).appendDependent(innerResults => {
          inst(ai.e, innerResults:_*)
        })

      case _ => throw new scala.NotImplementedError(s"No rule to convert $instance to the target language")
    }
  }

  // TODO: issues with other haskell implementations. must move to subclasses
  override def generateDataTypes(m:domain.Model): HaskellWithPath = {
    val allTypes = m.types.map(exp => {
      val params:Seq[HaskellType] = exp.attributes.map(att => typeConverter(att.tpe))
      val list:String = params.map(f => f.toString).mkString(" ")
      Haskell(s"${exp.concept}T $list") // not sure how much this is needed
    }).mkString("  | ")

    val binaryTreeInterface =  if (m.flatten().hasBinaryMethod()) {
      // astree method declaration
      definedDataSubTypes("", m.types) ++ declarations
    } else {
      Seq.empty
    }

    val code = Haskell(
      s"""|module DataTypes where
          |import GeneralExpr
          |${binaryTreeInterface.mkString("\n")}
          |
          |-- All types are classified as data
          |data ${domain.baseTypeRep.name} = $allTypes
          |""".stripMargin)

    HaskellWithPath(code, Paths.get("DataTypes.hs"))
  }

  /** Create multiple Haskell files for test cases. */
  override def generateSuite(model: Option[Model] = None): Seq[HaskellWithPath] = {
    val opsImports = flat.ops.map(op => s"import ${op.concept}").mkString("\n")
    val typesImports = flat.types.map(exp => s"import ${exp.concept}").mkString("\n")
    var num: Int = -1

    testGenerator.map(md => {
      num = num + 1
      HaskellWithPath(Haskell(s"""|module Main where
                                  |import Test.HUnit
                                  |import GeneralExpr
                                  |import Base
                                  |
                                  |$opsImports
                                  |$typesImports
                                  |$md""".stripMargin), Paths.get(s"Main$num.hs"))
    })
  }
}