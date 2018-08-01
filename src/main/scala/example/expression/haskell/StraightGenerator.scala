package example.expression.haskell    /*DI:LD:AD*/

import java.nio.file.Paths

import example.expression.domain.{BaseDomain, ModelDomain}

// https://eli.thegreenplace.net/2016/the-expression-problem-and-its-solutions/

trait StraightGenerator extends AbstractGenerator with StandardHaskellBinaryMethod {
  val domain:BaseDomain with ModelDomain
  import domain._

  def getModel: domain.Model

  lazy val flat:domain.Model = getModel.flatten()

  /** Return designated HaskellType. */
  override def typeConverter(tpe:domain.TypeRep, covariantReplacement:Option[HaskellType] = None) : HaskellType = {
    tpe match {
      case domain.baseTypeRep => covariantReplacement.getOrElse(new HaskellType(domain.baseTypeRep.name))
      case _ => super.typeConverter(tpe, covariantReplacement)
    }
  }

  /** For the processed model, return generated code artifacts for solution. */
  def generatedCode():Seq[HaskellWithPath] = {
      flat.ops.map(op => generateOp(flat, op)) :+
      generateDataTypes(flat)
  }

  def generateOp(m:Model, op:Operation) : HaskellWithPath = {
    val name = op.name
    val opRetType = typeConverter(op.returnType.get)
    val extraOp = op.parameters.map(tuple => {    // what happens when two params?
      val tpe = tuple._2
      if (tpe.equals(domain.baseTypeRep)) {
        s""" -> ${domain.baseTypeRep.name} """
      } else {
        tpe  // not sure what else to do
      }
    }).mkString("")

    val definition = Haskell(s"$name :: ${domain.baseTypeRep.name} $extraOp -> $opRetType")

    val instances = m.types.map(exp => {
      val opsParam = op.parameters.map(tuple => {    // what happens when two params?
        val name = tuple._1
        val tpe = tuple._2
        if (tpe.equals(domain.baseTypeRep)) {
          s"""(${exp.name.capitalize} ${standardArgs(exp, "2").getCode})"""
        } else {
          name  // not sure what else to do
        }
      }).mkString("")

      s"""$name (${exp.name.capitalize} ${standardArgs(exp).getCode}) $opsParam = ${logic(exp)(op).mkString("\n")}"""
    })

    val code = Haskell(s"""|module ${name.capitalize} where
                           |import DataTypes
                           |${addedImports(op).mkString("\n")}
                           |$definition
                           |${instances.mkString("\n")}""".stripMargin)
    HaskellWithPath(code, Paths.get(s"${name.capitalize}.hs"))
  }


  def generateDataTypes(m:Model): HaskellWithPath = {
    val allTypes = m.types.map(exp => {
      val params:Seq[HaskellType] = exp.attributes.map(att => typeConverter(att.tpe))
      val list:String = params.map(f => f.toString).mkString(" ")
      Haskell(s"${exp.name.capitalize} $list")
    }).mkString("  | ")

    val code = Haskell(
      s"""|module DataTypes where
          |
          |-- All types are classified as data
          |data ${domain.baseTypeRep.name} = $allTypes
          |""".stripMargin)

    HaskellWithPath(code, Paths.get("DataTypes.hs"))
  }

    /** Responsible for dispatching sub-expressions with possible parameter(s). */
    override def dispatch(primary:Haskell, op:domain.Operation, params:Haskell*) : Haskell = {
      val args:String = params.mkString(" ")

      Haskell(s"""(${op.name} ${primary.toString} $args)""")
    }

  /**
    * Determines the Haskell expression for all children of a Exp subtype based on its attributes.
    *
    * For example, an expressions.BinaryExp has 'left' and 'right' attributes, whereas an
    * expressions.UnaryExp only has an 'exp'
    */
  def subExpressions(exp:domain.Atomic) : Map[String, Haskell] = {
    exp.attributes.map(att => att.name -> Haskell(s"${att.name}")).toMap
  }
}