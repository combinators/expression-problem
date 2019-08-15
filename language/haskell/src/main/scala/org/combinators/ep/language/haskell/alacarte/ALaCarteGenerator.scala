package org.combinators.ep.language.haskell.alacarte    /*DI:LD:AD*/

import java.nio.file.Paths

import org.combinators.ep.language.haskell._
import org.combinators.ep.domain.BaseDomain

/**
  * Based on Data Types a la Carte
  * https://doi.org/10.1017/S0956796808006758
  */
trait ALaCarteGenerator extends HaskellGenerator with StandardHaskellBinaryMethod with HaskellBinaryMethod with ALaCarteProducer {
  val domain:BaseDomain with ModelDomain
  import domain._

  def getModel: domain.Model

  lazy val flat:domain.Model = getModel.flatten()

  /** Return designated HaskellType. Note GeneralExpr is most abstract.  */
  override def typeConverter(tpe:domain.TypeRep) : HaskellType = {
    tpe match {
      case domain.baseTypeRep => new HaskellType("GeneralExpr")
      case _ => super.typeConverter(tpe)
    }
  }

  override def contextDispatch(source:Context, delta:Delta) : Expression = {
    if (source.op.isEmpty) {
      val params = if (delta.params.nonEmpty) {
        // not neeede? delta.params.map(h => "(" + h.getCode + ")").mkString("(", " ", ")")
        //"(" + delta.params.mkString(" ") + ")"
        delta.params.mkString("("," ", ")")
      } else {
        ""
      }
       Haskell(s"(${delta.op.get.instance} (${delta.expr.get} :: GeneralExpr) $params)")
    } else if (delta.op.isDefined && !source.op.get.equals(delta.op.get)) {
      if (delta.expr.isEmpty) {
        // this is to SELF so, just invoke
        Haskell(s"REP_LACE")
      } else {
        Haskell(s"(${delta.op.get.instance} (${delta.expr.get}))")
      }
    } else {
      super.contextDispatch(source, delta)
    }
  }

  /** For the processed model, return generated code artifacts for solution. */
  def generatedCode():Seq[HaskellWithPath] = {

    flat.types.map(tpe => generateExp(flat, tpe)) ++
      flat.ops.map(op => generateOp(flat, op)) :+
      generateBase(flat) :+
      generateTypeUnifier(flat) :+
      generateDataTypes(flat)
  }

  def genTypes(exp:DataType) : Seq[Haskell] = {
    exp.attributes.map(att => {
      att.tpe match {
        case domain.baseTypeRep => Haskell("e")
        case _ => Haskell(att.tpe.toString)
      }
    })
  }

  def genInstances(exp:DataType) : Seq[Haskell] = {
    exp.attributes.zipWithIndex.map{ case (att, num) => {
      att.tpe match {
        case domain.baseTypeRep => Haskell("e" + num)
        case _ =>
          // Still not sure why can't use att.tpe.instance + num
          Haskell(att.tpe.toString.toLowerCase + num)
      }
    }}
  }

  // returns argument if non-recursive, otherwise applies function 'f'
  def genArguments(exp:DataType, funcName:String) : Seq[Haskell] = {
    exp.attributes.zipWithIndex.map{ case (att, num) => {
      att.tpe match {
        case domain.baseTypeRep => Haskell(s"($funcName e$num)")
        case _ =>
          // Still not sure why can't use att.tpe.instance + num
          Haskell(att.tpe.toString.toLowerCase + num)
      }
    }}
  }

  def generateExp(m:Model, exp:DataType) : HaskellWithPath = {
    val name = exp.concept
    val types:Seq[Haskell] = genTypes(exp)
    val instances:Seq[Haskell] = genInstances(exp)
    val args:Seq[Haskell] = genArguments(exp, "f")
    val applications:String = args.mkString(" ")
    val code = Haskell(s"""
         |module $name where
         |
         |data $name e = $name ${types.mkString(" ")}
         |
         |instance Functor $name where
         |  fmap f ($name ${instances.mkString(" ")}) = $name $applications
         |""".stripMargin)
    HaskellWithPath(code, Paths.get(s"$name.hs"))
  }

  def exprHelper(s:Seq[domain.DataType]) : String = {
    if (s.size == 1) {
      s.head.toString
    } else {
      "( ET " + s.head.toString + " " + exprHelper(s.tail) + ")"
    }
  }

  def generateOp(m:Model, op:Operation) : HaskellWithPath = {
    op match {
      case opb:domain.BinaryMethod => generateBinaryOp(m, op)
      case _ => generateStandardOp(m, op)
    }
  }

  def generateStandardOp(m:Model, op:Operation) : HaskellWithPath = {
    val name = op.concept
    val imports = m.types.map(tpe => Haskell(s"import ${tpe.concept}")).mkString("\n")
    val instances:Seq[Haskell] = m.types.map(exp => {
      val code = logic(exp, op).mkString("\n")
      Haskell(s""" |instance $name ${exp.toString} where
                   |  ${op.instance}OneLevel (${exp.toString} ${standardArgs(exp).getCode}) = $code""".stripMargin)
    })

    val opRetType = typeConverter(op.returnType)
    val dependencies = dependency(op).map(op => s"import ${op.concept}").mkString("\n")
    val code = Haskell(s"""|module $name where
                           |import Base
                           |import GeneralExpr    -- only needed for Producer operations
                           |${addedImports(op).mkString("\n")}
                           |$dependencies
                           |$imports
                           |
                           |class Functor f => $name f where
                           |  ${op.instance}OneLevel :: f $opRetType -> $opRetType
                           |${instances.mkString("\n")}
                           |
                           |instance ($name f, $name g) => $name (ET f g) where
                           |  ${op.instance}OneLevel  (El x) = ${op.instance}OneLevel  x
                           |  ${op.instance}OneLevel  (Er y) = ${op.instance}OneLevel  y
                           |
                           |${op.instance} :: $name f => Expr f -> ${typeConverter(op.returnType)}
                           |${op.instance} expr = foldExpr ${op.instance}OneLevel expr
                           |""".stripMargin)
    HaskellWithPath(code, Paths.get(s"$name.hs"))
  }

  def generateTypeUnifier(m:Model):HaskellWithPath = {
    val imports = m.types.map(tpe => Haskell(s"import ${tpe.name}")).mkString("\n")
    val code = Haskell(
      s"""|-- A type that unifies all three existing types
          |module GeneralExpr where
          |import Base
          |$imports
          |type GeneralExpr = Expr ${exprHelper(m.types)}""".stripMargin)
    HaskellWithPath(code, Paths.get("GeneralExpr.hs"))
  }

  def generateBase(m:Model): HaskellWithPath = {
    val code = Haskell(
      s"""|module Base where
          |
          |-- type parameter f is the signature of the constructors
          |data Expr f = In (f (Expr f))
          |data ET f g e = El (f e) | Er (g e)
          |
          |-- useful to perform fold on an expression
          |foldExpr :: Functor f => (f a -> a) -> Expr f -> a
          |foldExpr f (In t) = f (fmap (foldExpr f) t)
          |
          |-- coproduct ET is also a functor
          |instance (Functor f, Functor g) => Functor (ET f g) where
          |  fmap f (El e1) = El (fmap f e1)
          |  fmap f (Er e2) = Er (fmap f e2)""".stripMargin)

    HaskellWithPath(code, Paths.get("Base.hs"))
  }

  /**
    * Determines the Haskell expression for all children of a Exp subtype based on its attributes.
    *
    * For example, an expressions.BinaryExp has 'left' and 'right' attributes, whereas an
    * expressions.UnaryExp only has an 'exp'
    */
  override def expression (exp:domain.DataType, att:domain.Attribute) : Expression = {
    Haskell(s"${att.instance}")
  }

  /** Responsible for dispatching sub-expressions with possible parameter(s). */
 override def dispatch(primary:Haskell, op:domain.Operation, params:Haskell*) : Haskell = {
   op match {
     case b:BinaryMethod => {
       val args:String = if (params.isEmpty) {
         ""
       } else {
         params.map(h => "(" + h.getCode + ")").mkString(" ")
       }
       Haskell(s"""(${op.instance} (${primary.toString} :: GeneralExpr) $args)""")
     }
     case _ => Haskell(s"""$primary""")
   }
  }
}