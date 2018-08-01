package example.expression.haskell       /*DI:LD:AD*/

import java.nio.file.Paths

import example.expression.domain.{BaseDomain, ModelDomain}

trait ALaCarteGenerator extends AbstractGenerator {
  import domain._

  def getModel: domain.Model

  lazy val flat:domain.Model = getModel.flatten()

  /** Return designated HaskellType. Note GeneralExpr is most abstract.  */
  override def typeConverter(tpe:domain.TypeRep, covariantReplacement:Option[HaskellType] = None) : HaskellType = {
    tpe match {
      case domain.baseTypeRep => covariantReplacement.getOrElse(new HaskellType("GeneralExpr"))
      case _ => super.typeConverter(tpe, covariantReplacement)
    }
  }

  /** For the processed model, return generated code artifacts for solution. */
  def generatedCode():Seq[HaskellWithPath] = {

    flat.types.map(tpe => generateExp(flat, tpe)) ++
      flat.ops.map(op => generateOp(flat, op)) :+
      generateBase(flat) :+
      generateTypeUnifier(flat)
  }

  def genTypes(exp:Atomic) : Seq[Haskell] = {
    exp.attributes.map(att => {
      att.tpe match {
        case domain.baseTypeRep => Haskell("e")
        case _ => Haskell(att.tpe.toString)
      }
    })
  }

  def genInstances(exp:Atomic) : Seq[Haskell] = {
    var num:Int = 0
    exp.attributes.map(att => {
      num = num + 1
      att.tpe match {
        case domain.baseTypeRep => Haskell("e" + num)
        case _ => Haskell(att.tpe.toString.toLowerCase + num)
      }
    })
  }

  // returns argument if non-recursive, otherwise applies function 'f'
  def genArguments(exp:Atomic, funcName:String) : Seq[Haskell] = {
    var num:Int = 0
    exp.attributes.map(att => {
      num = num + 1
      att.tpe match {
        case domain.baseTypeRep => Haskell(s"($funcName e$num)")
        case _ => Haskell(att.tpe.toString.toLowerCase + num)
      }
    })
  }

  def generateExp(m:Model, exp:Atomic) : HaskellWithPath = {
    val name = exp.name.capitalize
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

  def exprHelper(s:Seq[domain.Atomic]) : String = {
    if (s.size == 1) {
      s.head.toString
    } else {
      "( ET " + s.head.toString + " " + exprHelper(s.tail) + ")"
    }
  }

//  /** Construct args list "a1 a2 a3 ..." */
//  def standardArgs(exp:Atomic) : String = {
//    val vals:Range = 1 to exp.attributes.size
//    vals.map(v => s"a$v").mkString (" ")
//  }

  def generateOp(m:Model, op:Operation) : HaskellWithPath = {
    val name = op.name.capitalize
    val imports = m.types.map(tpe => Haskell(s"import ${tpe.name}")).mkString("\n")
    val instances:Seq[Haskell] = m.types.map(exp => {
      val code = logic(exp)(op).mkString("\n")
      Haskell(s""" |instance $name ${exp.toString} where
                   |  ${op.name}Functor (${exp.toString} ${standardArgs(exp).getCode}) = $code""".stripMargin)
    })

    val opRetType = typeConverter(op.returnType.get)
    val code = Haskell(s"""|module $name where
                           |import Base
                           |import GeneralExpr    -- only needed for Producer operations
                           |${addedImports(op).mkString("\n")}
                           |$imports
                           |
                           |class Functor f => $name f where
                           |  ${op.name}Functor :: f $opRetType -> $opRetType
                           |${instances.mkString("\n")}
                           |
                           |instance ($name f, $name g) => $name (ET f g) where
                           |  ${op.name}Functor  (El x) = ${op.name}Functor  x
                           |  ${op.name}Functor  (Er y) = ${op.name}Functor  y
                           |
                           |${op.name} :: $name f => Expr f -> ${typeConverter(op.returnType.get)}
                           |${op.name} expr = foldExpr ${op.name}Functor expr
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
  override def subExpressions(exp:domain.Atomic) : Map[String, Haskell] = {
    exp.attributes.map(att => att.name -> Haskell(s"${att.name}")).toMap
  }

  /** Responsible for dispatching sub-expressions with possible parameter(s). */
 override def dispatch(primary:Haskell, op:domain.Operation, params:Haskell*) : Haskell = {
    val args:String = params.mkString(" ")
    Haskell(s"""$primary""")
  }
}