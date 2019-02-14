package org.combinators.ep.language.haskell     /*DI:LD:AI*/

import java.io.File
import java.nio.file.{Path, Paths}

import org.combinators.ep.domain.OperationDependency
import org.combinators.ep.generator.{LanguageIndependentGenerator, Producer}

/**
  * Any Haskell EP approach can extend this Generator
  *
  * Perhaps consider an Expression Problem application domain based on Monoids
  */
trait HaskellGenerator extends LanguageIndependentGenerator with StandardHaskellBinaryMethod with OperationDependency with HaskellBinaryMethod with Producer {

  /** Specially required files are placed in this area. */
  val haskellResources:String = Seq("src", "main", "resources", "haskell-code").mkString(File.separator)

  type CompilationUnit = HaskellWithPath
  type Type = HaskellType
  type Expression = Haskell
  type Statement = Haskell
  type InstanceExpression = Haskell

  /** Find the model which contains a given atomic inst. */
  def findModel (exp:domain.DataType) : domain.Model = {
    getModel.toSeq.filter(m => m.types.contains(exp)).head
  }

  /** Return designated HaskellType. */
  override def typeConverter(tpe:domain.TypeRep) : HaskellType = {
    tpe match {
      case domain.baseTypeRep => new HaskellType(domain.baseTypeRep.name)
      case _ => super.typeConverter(tpe)
    }
  }

  /**
    * Default behavior in Haskell is the expression itself
    */
  def result (expr:Expression) : Seq[Statement] = {
    Seq(expr)
  }


  /**
    * Haskell solutions require delegation to their respective traits
    */
  def inst(exp:domain.DataType, params:InstanceExpression*): InstanceExpression

  /** Concatenate attributes by name in order */
  def standardArgs(exp:domain.DataType, suffix:String = "") : Haskell = {
    Haskell(exp.attributes.map(att => att.instance + suffix).mkString(" "))
  }

  /** Create sequence of attributes, suitable for varargs declarations. */
  def standardVarArgs(exp:domain.DataType, suffix:String = "") : Seq[Haskell] = {
    exp.attributes.map(att => Haskell(att.instance + suffix))
  }

  /**
    * If any new imports are needed for an operation, just extend here.
    *
    * This is a distinctly different interface than the ability to declare when an operation
    * has a dependent operation upon which it depends; in that case, override the
    * dependency(op:Operation) method.
    *
    * @param op
    * @return
    */
  def addedImports(op:domain.Operation):Seq[Haskell] = Seq.empty

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
      Haskell(s"${exp.concept} $list") // not sure how much this is needed
    }).mkString("  | ")

    val binaryTreeInterface =  if (m.flatten().hasBinaryMethod()) {
      // astree method declaration
      definedDataSubTypes("", m.types) ++ declarations
    } else {
      Seq.empty
    }

    val code = Haskell(
      s"""|module DataTypes where
          |${binaryTreeInterface.mkString("\n")}
          |
          |-- All types are classified as data
          |data ${domain.baseTypeRep.name} = $allTypes
          |""".stripMargin)

    HaskellWithPath(code, Paths.get("DataTypes.hs"))
  }

  /** Taken from scala meta web page. */
  def loadSource(entry:String*) : HaskellWithPath = {
    val path:Path = java.nio.file.Paths.get(haskellResources, entry: _*)
    val contents = java.nio.file.Files.readAllBytes(path).map(_.toChar).mkString

    HaskellWithPath(Haskell(contents), Paths.get(entry.head, entry.tail : _*))
  }

  /**
    * Helpful snippet to get all regular files below a given directory, using
    * the specified header as the relative path to those files
    */
  def getRecursiveListOfFiles(dir: File, header:String*): Seq[HaskellWithPath] = {
    val these:Seq[File] = dir.listFiles
    if (these == null || these.isEmpty) {
      Seq.empty
    } else {
      val sources: Seq[HaskellWithPath] = these.filterNot(f => f.isDirectory).map(f => loadSource(header :+ f.getName: _*))

      sources ++ these.filter(_.isDirectory).flatMap(f => getRecursiveListOfFiles(f, header :+ f.getName: _*))
    }
  }

  /**
    * Helper artifacts to be loaded for Haskell.
    */
  def helperClasses():Seq[HaskellWithPath] = {
    getRecursiveListOfFiles(Paths.get(haskellResources).toFile)
  }

}
