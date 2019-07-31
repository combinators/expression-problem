package org.combinators.ep.language.haskell.grow     /*DI:LD:AD*/

import java.nio.file.Paths

import org.combinators.ep.language.haskell._
import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import org.combinators.ep.language.haskell.HaskellGenerator

/**
  * Based on Grow Haskell paper.
  * http://www.jucs.org/jucs_23_1/trees_that_grow/jucs_23_01_0042_0062_najd.pdf
  *
  * Most complicated part of this code is working with expanded functions:
  *
  * helpEval is to be used for the dependent operation, Eval, within Simplify
  * help is to be used for just "lifting" a child to be the simplification:

{{{let
    leftVal = \${contextDispatch(source, deltaLeft)}
    rightVal = \${contextDispatch(source, deltaRight)}
  in if leftVal == 0 || rightVal == 0.0
    then \${result(inst(Lit, zero)).mkString("\n")}
    else if leftVal == 1
      then \${result(dispatch(expression(exp,base.right), op)).mkString("\n")}
      else if rightVal == 1
        then \${result(dispatch(expression(exp,base.left), op)).mkString("\n")}
        else \${result(inst(Mult, standardVarArgs(Add) : _*)).mkString("\n")}
}}}

  * contextDispatch results in "help\${delta.op.get.concept} \${delta.expr.get}"
  *
  * dispatch results in "(\${op.instance}\${domain.baseTypeRep.name} helpWith \${primary.toString} \$args)"
  *
  * so look for strings "helpEval" and "helpWith". For example, logic is expanded as follows:


{{{
  let
    leftVal = helpEval left
    rightVal = helpEval right
  in if leftVal == 0 || rightVal == 0.0
    then Lit 0.0
    else if leftVal == 1
      then (simplifyExp helpWith right ) --  CHANGE to just help right
      else if rightVal == 1
        then (simplifyExp helpWith left )
        else (Ext_M0 ((Ext_M1 (Ext_M2 (Mult (left) (right) )))))
}}}

  * And this must then be expanded into following by surrounding with a larger 'let' that
  * defines 'helpEval' and provides a meaningful substitute (and telescoping) implementation
  * of 'help' that recognizes the need to (recursively) apply both simplify and eval because
  * of the inherent dependency between Simplify and Eval.
  *
  * Also one must replace "\${op.instance}Exp helpWith " with "help "
  *
  * Since Simplify depends upon Eval, the initial signature needs two params (helpWithEval
  * and helpWith), both of which are used within the inner definition for 'help'

{{{
simplifyExp_M3 helpWithEval helpWith (Mult left right) =
  let
    help = (simplifyExp_M0 (evalExp_M1 (evalExp_M2 (evalExp_M3 helpWithEval)))
                             (simplifyExp_M1 (evalExp_M2 (evalExp_M3 helpWithEval))
                                             (simplifyExp_M2 (evalExp_M3 helpWithEval)
                                                             (simplifyExp_M3 helpWithEval helpWith))))
    helpEval = evalExp_M0 (evalExp_M1 (evalExp_M2 (evalExp_M3 helpWithEval)))
  in let
       leftVal = helpEval left   -- contextDispatch(source, dispatchChild(source.e, domain.base.left, Eval))
       rightVal = helpEval right
     in if leftVal == 0 || rightVal == 0.0
         then Lit 0.0
         else if leftVal == 1
              then (help right )   -- result(dispatch(expression(exp,base.right), op)).mkString("\n")
              else if rightVal == 1
                   then (help left )
                   else (Ext_M0 ((Ext_M1 (Ext_M2 (Mult (left) (right) )))))
}}}

  The proper repackaging is done in "generateOp" where the invocation to logic (from above) is
  manipulated via string-rewriting, based on the current "model" level.

  */
trait GrowGenerator extends HaskellGenerator with StandardHaskellBinaryMethod with HaskellBinaryMethod {
  val domain:BaseDomain with ModelDomain
  import domain._

  def getModel: domain.Model

  lazy val flat:domain.Model = getModel.flatten()

  /** For the processed model, return generated code artifacts for solution. */
  def generatedCode():Seq[HaskellWithPath] = {
    getModel.inChronologicalOrder.map(m => generateEvolution(m)) :+
    generateDataTypes(flat)
  }

  /** Return designated HaskellType. */
  override def typeConverter(tpe:domain.TypeRep) : HaskellType = {
    tpe match {
      case domain.baseTypeRep => new HaskellType(s"${expDeclaration(getModel.base())} f")
      case _ => super.typeConverter(tpe)
    }
  }

  override def inst(exp:domain.DataType, params:Expression*): CodeBlockWithResultingExpressions = {
    val args = params.mkString(",")

    val wrap = genWrap(findModel(exp))
    val inner = exp match {
      case ui: Unary =>
        Haskell(wrap(s"${ui.concept} (${params(0)}) "))

      case bi: Binary =>
        Haskell(wrap(s"${bi.concept} (${params(0)}) (${params(1)}) "))

      case exp: Atomic =>
        Haskell(wrap(s"${exp.concept} ${params(0)} "))

      case _ => Haskell(s" -- unknown ${exp.concept} ")
    }

    CodeBlockWithResultingExpressions(inner)
  }

  /**
    * Extended to handle producer operations specially.
    *
    * @param m
    * @param op
    * @return
    */
  def typeSignature(m:Model, op:Operation) : String = {
    op match {
      case _:ProducerOperation =>
        val mcaps = m.name.capitalize    // haskell needs data to be capitalized!
      val baseDomain = domain.baseTypeRep.name

        s"${op.instance}$baseDomain$mcaps :: ${expDeclaration(m.base())} $mcaps -> ${expDeclaration(m.base())} $mcaps"

      case _:BinaryMethod =>
        val mcaps = m.name.capitalize    // haskell needs data to be capitalized!
      val baseDomain = domain.baseTypeRep.name

        val result = typeConverter(op.returnType)
        s"${op.instance}$baseDomain$mcaps :: ${expDeclaration(m.base())} $mcaps -> ${expDeclaration(m.base())} $mcaps -> $result"

      case _ =>
        val mcaps = m.name.capitalize    // haskell needs data to be capitalized!
      val baseDomain = domain.baseTypeRep.name

        s"${op.instance}$baseDomain$mcaps :: ${expDeclaration(m.base())} $mcaps -> ${typeConverter(op.returnType)}"
    }
  }

  /** Combined string from the types. */
  def extTypeDeclaration(m:Model):String = {
    extDeclaration(m) + "Type"
  }

  def onlyTypes(m:Model):String = {
    m.types.map(t => t.concept).mkString("")
  }

  /** Combined string from the types. */
  def extDeclaration(m:Model):String = {
    "Ext_" + m.name.capitalize
  }

  /** Exp defined solely by types. */
  def expDeclaration(m:Model):String = {
    domain.baseTypeRep.name + "_" + m.name.capitalize
  }

  /**
    * If there are dependent operations, the resulting invocations become all the more complicated...
    *
    * @param m
    * @param op
    * @param depends
    * @return
    */
  def operationForFixedLevel(m:Model, op:Operation, depends:Seq[Operation]) : String = {
    val mcaps = m.name.capitalize // haskell needs data to be capitalized!
    val baseDomain = domain.baseTypeRep.name

    // BAD
    // (simplifyExp_M0 (simplifyExp_M1 (simplifyExp_M2 (simplifyExp_M3 (simplifyExp_M4 helpWithSimplifyM4))))) e

    // GOOD
    //    (simplifyExp_M0 (evalExp_M1 (evalExp_M2 (evalExp_M3 (evalExp_M4 helpWithEvalM4))))
    //    (simplifyExp_M1 (evalExp_M2 (evalExp_M3 (evalExp_M4 helpWithEvalM4)))
    //    (simplifyExp_M2 (evalExp_M3 (evalExp_M4 helpWithEvalM4))
    //    (simplifyExp_M3 (evalExp_M4 helpWithEvalM4)
    //    (simplifyExp_M4 helpWithEvalM4 helpWithSimplifyM4))))) e
    val allModels = m.inChronologicalOrder

    if (depends.isEmpty || depends.size > 1) {
      val invoke = allModels.reverse.tail.foldLeft(s"(${op.instance}${expDeclaration(m)} helpWith${op.concept}$mcaps)")((former, next) => {
        s"(${op.instance}${expDeclaration(next)} $former)"
      })

      s"""
         #${typeSignature(m, op)}
         #${op.instance}$baseDomain$mcaps e = $invoke e
         #""".stripMargin('#')
    } else {
      val dop = depends.head

      val invoke = allModels.indices.map(outer => {
        val lastOne = if (outer == allModels.size-1) { s"helpWith${op.concept}$mcaps" } else {""}
        s"(${op.instance}${expDeclaration(allModels(outer))} " + // inner goes from outer+1 .. Mn
          (allModels.size - 1 until outer by -1).foldLeft(s" helpWith${dop.concept}$mcaps")((state, inner) => s"(${dop.instance}${expDeclaration(allModels(inner))} $state)").mkString("") +
          s" $lastOne"  // ONLY LAST one has "helpWith${op.concept}$mcaps"
      }).mkString("") + allModels.indices.map(_ => ")").mkString("")

//      val invoke = allModels.reverse.tail.foldLeft(s"(${op.instance}${expDeclaration(m)} helpWith${dop.concept}$mcaps helpWith${op.concept}$mcaps)")((outerFormer, outerNext) => {
//
//      val mcaps = m.name.capitalize
//      val inner = outerNext.inChronologicalOrder.reverse.tail.foldLeft(s"(${dop.instance}Exp_$mcaps helpWith${dop.concept}$mcaps)")((former, next) => {
//          s"(${dop.instance}Exp_${next.name.capitalize} $former)"})
//        s"(${op.instance}${expDeclaration(outerNext)} $inner $outerFormer)"
//      })

      s"""
         #${typeSignature(m, op)}
         #${op.instance}$baseDomain$mcaps e = $invoke e
         #""".stripMargin('#')
    }
  }

  def binaryOperationForFixedLevel(m:Model, op:Operation, depends:Seq[Operation]) : String = {
    val mcaps = m.name.capitalize // haskell needs data to be capitalized!
    val baseDomain = domain.baseTypeRep.name

    val allModels = m.inChronologicalOrder

 //   if (depends.isEmpty || depends.size > 1) {
      val invoke = allModels.reverse.tail.foldLeft(s"(${op.instance}${expDeclaration(m)} helpWith${op.concept}$mcaps)")((former, next) => {
        s"(${op.instance}${expDeclaration(next)} $former)"
      })

      s"""
         #${typeSignature(m, op)}
         #${op.instance}$baseDomain$mcaps e1 e2 = $invoke e1 e2
         #""".stripMargin('#')
//    } else {
//      val dop = depends.head
//
//      val invoke = allModels.indices.map(outer => {
//        val lastOne = if (outer == allModels.size-1) { s"helpWith${op.concept}$mcaps" } else {""}
//        s"(${op.instance}${expDeclaration(allModels(outer))} " + // inner goes from outer+1 .. Mn
//          (allModels.size - 1 until outer by -1).foldLeft(s" helpWith${dop.concept}$mcaps")((state, inner) => s"(${dop.instance}${expDeclaration(allModels(inner))} $state)").mkString("") +
//          s" $lastOne"  // ONLY LAST one has "helpWith${op.concept}$mcaps"
//      }).mkString("") + allModels.indices.map(_ => ")").mkString("")
//
//      s"""
//         #${typeSignature(m, op)}
//         #${op.instance}$baseDomain$mcaps e1 e2 = $invoke e1 e2
//         #""".stripMargin('#')
//    }
  }

  /**
    * Generates wrapper of instantiations using telescoping constructors
    *
    * @param model
    * @return
    */
  def genWrap(model: Model) : String => String = {
    if (model.base() == model) {
      (s:String) => s
    } else {
      (s:String) => {
        model.last.inChronologicalOrder.reverse.tail.foldLeft(s"${extDeclaration(model.last)} ($s)")((former,tail) =>
          s"(${extDeclaration(tail)} ($former))")
      }
    }
  }

  /**
    * Handles refinement of SubExp f ~ ExpExtType for all predecessor types
    *
    * @param m
    * @param op
    * @return
    */
  def generateOp(m:Model, op:Operation) : Haskell = {
    val mcaps = m.name.capitalize    // haskell needs data to be capitalized!

    val baseDomain = domain.baseTypeRep.name
    //val name = op.name

    val returnType = typeConverter(op.returnType)
    val extType = extTypeDeclaration(m)

    // If an operation is a binary method ,add it in, or if it has a dependent operation. Needs cleaning up
    val extraParam = if (dependency(op).size == 1) {
      "_"
    } else {
     op match {
        case _: BinaryMethod => "_"
        case _ => ""
      }
    }

    // keep track of (singular) dependent operation; fall-back to self-op if no dependencies.
    val dop = if (dependency(op).size == 1) {
      dependency(op).head
    } else {
      op
    }

    // If an operation has a single dependency, then add it in
    val extraWith = if (dependency(op).size == 1) {
      s"helpWith${dop.concept}"
    } else {
      ""
    }

    val allModels = m.inChronologicalOrder
    val inner:String= m.types.map(exp => {
      val head = exp match {
        case b: Binary => s"${op.instance}${expDeclaration(m)} $extraWith helpWith "
        case u: Unary => s"${op.instance}${expDeclaration(m)} $extraWith helpWith "
        case _ => s"${op.instance}${expDeclaration(m)} _ $extraParam "
      }

      // how to handle binary operations?

      val modifiedRest = {
        // must embed 'help' properly, if needed.
        val code = logic(exp, op).mkString("\n")
        if (code.contains(" help")) { // This sure looks like a hack. How to figure out from code?

          // outer goes from M0 .. M1 .. Mn
          val help = if (dependency(op).isEmpty) {
            ""
          } else {

            "help = " + allModels.indices.map(outer => {
              val lastOne = if (outer == allModels.size-1) { "helpWith" } else {""}
              s"(${op.instance}${expDeclaration(allModels(outer))} " + // inner goes from outer+1 .. Mn
                (allModels.size - 1 until outer by -1).foldLeft(s" helpWith${dop.concept}")((state, inner) => s"(${dop.instance}${expDeclaration(allModels(inner))} $state)").mkString("") +
                   s" $lastOne"  // ONLY LAST one has "helpWith"
            }).mkString("") + allModels.indices.map(_ => ")").mkString("")
          }

          if (!m.last.isEmpty) {
            // convert "(simplifyExp helpWith right )" into "(help right )"
            val helpDOP = if (help.isEmpty) {
              val invokeSimple = allModels.reverse.foldLeft(s" helpWith")((outerState,outerNext) =>
                s"(${dop.instance}${expDeclaration(outerNext)} $outerState)")
              s"help = $invokeSimple"
            } else {
              val invoke = allModels.reverse.foldLeft(s" helpWith${dop.concept}")((outerState,outerNext) =>
                s"(${dop.instance}${expDeclaration(outerNext)} $outerState)")
              s"""
                 #    $help
                 #    help${dop.concept} = $invoke
               """.stripMargin('#')
            }

            s"""(${exp.concept} ${standardArgs(exp).getCode}) =
               #  let
               #    $helpDOP
               #  in
               #  ${code.replace(s"${op.instance}${domain.baseTypeRep.name} helpWith ", "help ")}""".stripMargin('#')
          } else {
            val name = if (dependency(op).size == 1) {
              dependency(op).head.instance
            } else {
              op.instance
            }

            val helpDOP = if (help.isEmpty) {
              s"help = ${op.instance}${expDeclaration(m)} helpWith"
            } else {
              s"""
                 #    $help
                 #    help${dop.concept} = $name${expDeclaration(m)} helpWith${dop.concept}
               """.stripMargin('#')
            }

            s"""(${exp.concept} ${standardArgs(exp).getCode}) =
               #  let
               #    $helpDOP
               #  in
               #  ${code.replace(s"${op.instance}${domain.baseTypeRep.name} helpWith ", "help ")}""".stripMargin('#')
          }
        } else {
          val extraParam = op match {
            case _: BinaryMethod => "_"
            case _ => ""
          }
          s"(${exp.concept} ${standardArgs(exp).getCode} $extraParam) = " + code
        }
      }

      val modifiedBinaryRest = {
        // must embed 'help' properly, if needed.
        val code = logic(exp, op).mkString("\n")
        val extraParam = op match {
          case _: BinaryMethod => s"(${exp.concept} ${standardArgs(exp, "2").getCode})"
          case _ => ""
        }
        if (code.contains(" help")) { // This sure looks like a hack. How to figure out from code?
          if (!m.last.isEmpty) {
            // convert "(simplifyExp helpWith right )" into "(help right )"
            val helpOP = {
              val invokeSimple = allModels.reverse.foldLeft(s" helpWith")((outerState,outerNext) =>
                s"(${op.instance}${expDeclaration(outerNext)} $outerState)")
              s"help = $invokeSimple"
            }

            s"""(${exp.concept} ${standardArgs(exp).getCode}) $extraParam =
               #  let
               #    $helpOP
               #  in
               #  ${code.replace(s"${op.instance}${domain.baseTypeRep.name} helpWith ", "help ")}""".stripMargin('#')
          } else {
            val name = if (dependency(op).size == 1) {
              dependency(op).head.instance
            } else {
              op.instance
            }

            val helpDOP = s"help = (${op.instance}${expDeclaration(m)} helpWith)"

            s"""(${exp.concept} ${standardArgs(exp).getCode}) $extraParam =
               #  let
               #    $helpDOP
               #  in
               #  ${code.replace(s"${op.instance}${domain.baseTypeRep.name} helpWith ", "help ")}""".stripMargin('#')
          }
        } else {
          s"(${exp.concept} ${standardArgs(exp).getCode}) $extraParam = " + code
        }
      }

      val binaryHead = exp match {
        case b: Binary => s"${op.instance}${expDeclaration(m)} helpWith "
        case u: Unary => s"${op.instance}${expDeclaration(m)} helpWith "
        case _ => s"${op.instance}${expDeclaration(m)} _  "
      }

      // choose which one to use
      op match {
        case _: BinaryMethod => binaryHead + modifiedBinaryRest
        case _ => head + modifiedRest
      }

    }).mkString("\n")

    val previous:String = if (m.last.isEmpty) {
      "Void"
    } else {
      extTypeDeclaration(m) + " " + m.name.capitalize
    }

    // capture inner extension relationships
    val header = s"($extType f -> $returnType)"
    val precursor = if (!m.last.isEmpty) {
      // Must remove the lastmost "empty" one, as well as the one before it, since we don't need ~ arguments
      // for the first definition in M0
      val prior = m.toSeq.filterNot(m => m.isEmpty || m.last.isEmpty).map(m =>
        s"${expDeclaration(m)} f ~ ${extTypeDeclaration(m.last)} f")

      prior.mkString("(", ",", ")") + " => "
    } else {
      ""
    }

    val extraSignature = if (dependency(op).size == 1) {
      val ret = typeConverter(dop.returnType).tpe
      s"""#(Ext_${mcaps}Type f -> $ret)
          #  -- ^ Function to help with evaluating subexpression extensions
          #  -> """.stripMargin('#')
    } else {
      ""
    }

    // if we define new operations, we must expand as provided. Operations with dependent operations
    // have to be handled specially...
    val operationSpec = operationForFixedLevel(m, op, dependency(op))
    val binaryOperationSpec = binaryOperationForFixedLevel(m, op, dependency(op))

    //val binaryHeader = s"($extType f -> $extType f -> $returnType)"
    val extraBinarySignature =
      s"""#(Ext_${mcaps}Type f -> Ext_${mcaps}Type f -> Bool)
         #  -- ^ Function to help with evaluating subexpression extensions""".stripMargin('#')

    // Binary operations are different
    val binaryOp = Haskell(
      s"""
         #-- | Evaluates binary expression.
         #${op.instance}${expDeclaration(m)}
         #  :: $precursor$extraBinarySignature
         #  -> ${expDeclaration(m)} f
         #  -- ^ The first expression to evaluate
         #  -> ${expDeclaration(m)} f
         #  -- ^ The second expression to evaluate
         #  -> Bool
         #
         #$inner
         #${op.instance}${expDeclaration(m)} helpWith (${extDeclaration(m)} inner) (${extDeclaration(m)} inner2) = helpWith inner inner2
         #${op.instance}${expDeclaration(m)} _ _ _ = False
         #
         #-- | Evaluates an $mcaps expression
         #-- | Calls ${op.instance}$baseDomain with the $mcaps helper
         #$binaryOperationSpec
         #
         #-- | Helps with extensions $mcaps
         #helpWith${op.concept}$mcaps :: Void -> Void -> ${typeConverter(op.returnType)}
         #helpWith${op.concept}$mcaps = absurd
         #
         #""".stripMargin('#')
    )

    op match {
      case _ : BinaryMethod =>
        println (op.name + " is binary")
        binaryOp

      case _ =>
        println (op.name + " is normal")

    new Haskell(s"""
                   #-- | Evaluates expression.
                   #${op.instance}${expDeclaration(m)}
                   #  :: $precursor$extraSignature$header
                   #  -- ^ Function to help with extensions
                   #  -> ${expDeclaration(m)} f
                   #  -- ^ The expression to evaluate
                   #  -> $returnType
                   #
                   #$inner
                   #${op.instance}${expDeclaration(m)} $extraParam helpWith (${extDeclaration(m)} inner) = helpWith inner
                   #
                   #-- | Evaluates an $mcaps expression
                   #-- | Calls ${op.instance}$baseDomain with the $mcaps helper
                   #$operationSpec
                   #
                   #-- | Helps with extensions $mcaps
                   #helpWith${op.concept}$mcaps :: Void -> ${typeConverter(op.returnType)}
                   #helpWith${op.concept}$mcaps = absurd
                   #
                   #""".stripMargin('#'))
    }
  }

  def generateData(m:Model):Haskell = {
    val mcaps = m.name.capitalize    // haskell needs data to be capitalized!
    val Exp = expDeclaration(m.base())    //   domain.baseTypeRep.name + "_" + m.name.capitalize

    val inner:String= m.types.map( {
        case b:Binary => s"""${b.concept} ($Exp f) ($Exp f)     -- Binary instance """
        case c:Unary =>  s"""${c.concept} ($Exp f)              -- Unary instance """
        case a:Atomic => s"""${a.concept} ${typeConverter(a.attributes.head.tpe)}    -- Atomic instance """
      }
    ).mkString("\n     | ")

    val priorOps:String = if (m.ops.nonEmpty) {
      m.inChronologicalOrder.reverse.tail.reverse.flatMap(priorM => {
        m.ops.map(op => s"-- ${priorM.name.capitalize} part for ${op.instance}\n" + generateOp(priorM, op) + s"-- DONE ${priorM.name.capitalize} part\n")}).mkString("\n")
    } else { "" }

    val ops:String = m.ops.map(op => generateOp(m, op)).mkString("\n")

    var pastExtensions:String = ""
    var now = m
    while (!now.last.isEmpty) {
      val past = now.last
      pastExtensions = s"type instance ${extTypeDeclaration(past)} $mcaps = ${expDeclaration(now)} $mcaps\n" + pastExtensions
      now = now.last
    }

    // must find PAST operations and incorporate them here
    val pastOps = m.last.pastOperations().map(op => generateOp(m, op)).mkString("\n")

    val dataTypeDefinition = if (m.types.isEmpty) {
      s"newtype ${expDeclaration(m)} f = ${extDeclaration(m)} (${extTypeDeclaration(m)} f)"
    } else {
      s"""
         #data ${expDeclaration(m)} f = $inner
         #  | ${extDeclaration(m)} (${extTypeDeclaration(m)} f)    -- Datatype extensions""".stripMargin('#')
    }

    new Haskell(s"""
                   #-- | Datatype
                   #-- | Parameter f is to be filled with the marker type of the
                   #-- | current evolution.
                   #$dataTypeDefinition
                   #
                   #-- | Family of Exp data-type extensions:
                   #-- | Given a marker type of a evolution, compute the type extension
                   #-- | of Exp used for this evolution.
                   #type family ${extTypeDeclaration(m)} f
                   #
                   #$priorOps
                   #$ops
                   #
                   #-- Evolution $mcaps
                   #-- | Marker type to select evolution $mcaps
                   #data $mcaps
                   #
                   #-- | Selecting $mcaps means: no extensions to type ${expDeclaration(m)}; take care of previous ones
                   #$pastExtensions
                   #type instance ${extTypeDeclaration(m)} $mcaps = Void
                   #
                   #$pastOps
                   #""".stripMargin('#'))   // HACK: Issue with "|"
  }

  /**
    * Each evolution has chance to add data extensions and functional extensions
    *
    * @param m
    * @return
    */
  def generateEvolution(m:Model) : HaskellWithPath = {

    var pastImports:String = ""

    var past = m.last
    while (!past.isEmpty) {
      pastImports = s"import ${past.name.capitalize}\n" + pastImports
      past = past.last
    }

    // HACK: Awkward to use stripmargin, since generateData might start with "|" char in Haskell!!
    // no warning overlapping patterns is for Grow implementation
    val code = Haskell(s"""|{-# LANGUAGE TypeFamilies #-}
                           |{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
                           |
                           |module ${m.name.capitalize} where
                           |import DataTypes
                           |import GHC.Types (Constraint)
                           |import Data.Void
                           |$pastImports
                           |""".stripMargin + generateData(m))

    HaskellWithPath(code, Paths.get(s"${m.name.capitalize}.hs"))
  }

  /**
    * Responsible for dispatching sub-expressions with possible parameter(s).
    * Seems safest to include/embed parens here.
    *
    * middle operator is 'helpWith' or 'help' for extensions, and this needs to be 'fixed' by
    * the one calling logic. This is not an ideal solution but it works.
    *
    */
  override def dispatch(primary:Haskell, op:domain.Operation, params:Haskell*) : Haskell = {
    val args:String = params.mkString("")

    Haskell(s"""(${op.instance}${domain.baseTypeRep.name} helpWith ${primary.toString} $args)""")
  }

  /** For straight design solution, directly access attributes by name. */
  override def expression (exp:DataType, att:Attribute) : Expression = {
    Haskell(s"${att.instance}")
  }

  /**
    * HACK. TODO: FIX
    * Grow generateDT only needs first half...
    * @param m
    * @return
    */
  override def generateDataTypes(m:domain.Model): HaskellWithPath = {
    val binaryTreeInterface =  if (m.flatten().hasBinaryMethod) {
      definedDataSubTypes("", m.types) ++ declarations
    } else {
      Seq.empty
    }

    val code = Haskell(
      s"""|module DataTypes where
          |${binaryTreeInterface.mkString("\n")}
          |""".stripMargin)

    HaskellWithPath(code, Paths.get("DataTypes.hs"))
  }

  /** Find Model entry in the past that defines type. */
  def findType(m:Model, name:String) : Model = {
    if (m.isEmpty || m.types.exists(dt => dt.name.equals(name))) {
      m
    } else {
      findType(m.last, name)
    }
  }

  /**
    * Given a string expression "(Mult (Lit 1.0) (Lit 12.0))" Find the highest level in model that is needed to define these.
    *
    * @param s
    * @return
    */
  def retrieveHighestDataTypeLevel (s:String) : Model = {

    // get all non empty models.
    val all = getModel
    val allFound:Seq[Model] = s.replaceAll("\\(", " ").replaceAll("\\)", " ").split(" ").map(name => findType(all, name)).distinct.filterNot(m => m.isEmpty)

    // get all non-empty former models.
    val previous = allFound.map(m => m.last).distinct.filterNot(m => m.isEmpty)

    // find the only one that exists
    val distinct = allFound.filterNot(m => previous.contains(m)).distinct

    distinct.head
  }

  /**
    * When a method is delegated to a different context, relies no helper helper methods
    * that are constructed for this purpose.
    *
    * @param source     The source context where dispatch occurs
    * @param delta      The delta context that describes desired expression and operation
    *
    * @return
    */
  override def contextDispatch(source:Context, delta:Delta) : Expression = {
    if (delta.op.isDefined) {
      val op = delta.op.get
      // if an operation has a dependency, that is expressed with a helper function
      val sop:Option[Operation] = source.op
      val isBinary:Boolean = delta.op.get match {
        case bm:BinaryMethodTreeBase => true
        case _ => false
      }

      if (sop.isDefined && !sop.equals(delta.op)) {
        Haskell(s"""help${op.concept} ${delta.expr.get}""")
      } else if (sop.isDefined && isBinary) {
        // post-processing will resolve this
        Haskell(s"""help ${delta.expr.get}""")
      } else {
        Haskell(s"(${op.instance}${domain.baseTypeRep.name}${getModel.name.capitalize} (${delta.expr.get.toString}))")
      }
    } else {
      super.contextDispatch(source, delta)
    }
  }
}
