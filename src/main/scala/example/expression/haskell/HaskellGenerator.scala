package example.expression.haskell     /*DI:LD:AI*/

import java.nio.file.Paths

import example.expression.generator.LanguageIndependentGenerator

/**
  * Any Haskell EP approach can extend this Generator
  *
  * Perhaps consider an Expression Problem application domain based on Monoids
  */
trait HaskellGenerator extends LanguageIndependentGenerator with StandardHaskellBinaryMethod with HaskellBinaryMethod {

  type CompilationUnit = HaskellWithPath
  type Type = HaskellType
  type Expression = Haskell
  type Statement = Haskell

  /** Return designated HaskellType. */
  override def typeConverter(tpe:domain.TypeRep) : HaskellType = {
    tpe match {
      case domain.baseTypeRep => new HaskellType(domain.baseTypeRep.name)
      case _ => super.typeConverter(tpe)
    }
  }

  /** Concatenate attributes by name in order */
  def standardArgs(exp:domain.Atomic, suffix:String = "") : Haskell = {
    Haskell(exp.attributes.map(att => att.name + suffix).mkString(" "))
  }

  /** If any new imports are needed for an operation, just extend here. */
  def addedImports(op:domain.Operation):Seq[Haskell] = Seq.empty

  /**
    * Responsible for delegating to a new operation on the current context.
    */
  def delegate(exp:domain.Atomic, op:domain.Operation, params:Haskell*) : Haskell = {
    new Haskell("haskellReplaceMe")
  }

  /**
    * By default, each operation is fully specified and doesn't need any default, however for binary
    * methods, such as equals, there quite often needs to be a fall-through default case.
    */
  def requireDefault(op:domain.Operation) : Option[(Int,Haskell)] = None
  /**
    * Standard default value that throws exception in Haskell.
    */
  def standardDefault(tpe:domain.TypeRep) : Haskell = new Haskell("undefined")

  /**
    * Use this for default cases, such as
    *
    * equals _ _ = False
    */
  def defaultCase(functionName:Haskell, numParams:Int, defaultExpression:Haskell) : Haskell = {
    val bars = (1 to numParams).map(d => "_").mkString(" ")
    new Haskell(s"$functionName $bars = $defaultExpression")
  }


  def generateDataTypes(m:domain.Model): HaskellWithPath = {
    val allTypes = m.types.map(exp => {
      val params:Seq[HaskellType] = exp.attributes.map(att => typeConverter(att.tpe))
      val list:String = params.map(f => f.toString).mkString(" ")
      Haskell(s"${exp.name.capitalize} $list")
    }).mkString("  | ")

    val binaryTreeInterface =  if (m.flatten().ops.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      // astree method declaration
      definedDataSubTypes("", m.types) ++ declarations
    } else {
      Seq.empty
    }

    val code = Haskell(
      s"""|module DataTypes where
          |
          |${binaryTreeInterface.mkString("\n")}
          |
          |-- All types are classified as data
          |data ${domain.baseTypeRep.name} = $allTypes
          |""".stripMargin)

    HaskellWithPath(code, Paths.get("DataTypes.hs"))
  }

}
