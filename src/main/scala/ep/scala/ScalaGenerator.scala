package ep.scala   /*DI:LD:AI*/

import java.io.File
import java.nio.file.{Path, Paths}

import ep.domain.{BaseDomain, ModelDomain}
import ep.generator.{FileWithPath, LanguageIndependentGenerator, Producer}

/**
  * Any Scala-based EP approach can extend this Generator
  */
trait ScalaGenerator extends LanguageIndependentGenerator with Producer {
  val domain:BaseDomain with ModelDomain

  type CompilationUnit = ScalaWithPath
  type Type = scala.meta.Type
  type Expression = scala.meta.Term
  type Statement = scala.meta.Stat
  type InstanceExpression = scala.meta.Term

  /** Return designated Java type associated with type, or void if all else fails. */
  override def typeConverter(tpe:domain.TypeRep) : Type = {
    tpe match {
      case domain.baseTypeRep => scala.meta.Type.Name(domain.baseTypeRep.name)
      case _ => super.typeConverter(tpe)
    }
  }

  /**
    * Default behavior in Scala is to return an expression value as is
    */
  def result (expr:Expression) : Seq[Statement] = {
    Seq(expr)
  }

  /**
    * For producer operations, there is a need to instantiate objects, and one would use this
    * method (with specific parameters) to carry this out.
    */
  def inst(exp:domain.Atomic, params:InstanceExpression*): InstanceExpression = {
    Scala("new " + exp.concept + "(" + params.map(expr => expr.toString).mkString(",") + ")").expression
  }

  /**
    * Specially required files are placed in this area.
    *
    * Currently "build.sh"
    */
  val scalaResources:String = Seq("src", "main", "resources", "scala-resources").mkString(File.separator)

  /** Retrieve the contents of these files. */
  def loadSource(entry:String*) : FileWithPath = {
    val path:Path = java.nio.file.Paths.get(scalaResources, entry: _*)
    val contents = java.nio.file.Files.readAllBytes(path).map(_.toChar).mkString

    FileWithPath(contents, Paths.get(entry.head, entry.tail : _*))
  }

  /**
    * Helpful snippet to get all regular files below a given directory, using
    * the specified header as the relative path to those files.
    */
  def getRecursiveListOfFiles(dir: File, header:String*): Seq[FileWithPath] = {
    val these:Seq[File] = dir.listFiles
    if (these == null || these.isEmpty) {
      Seq.empty
    } else {
      val sources: Seq[FileWithPath] = these.filterNot(f => f.isDirectory).map(f => loadSource(header :+ f.getName: _*))

      sources ++ these.filter(_.isDirectory).flatMap(f => getRecursiveListOfFiles(f, header :+ f.getName: _*))
    }
  }

  /**
    * Retrieve Scala build.sbt file.
    */
  def getsbt():Seq[FileWithPath] = {
    getRecursiveListOfFiles(Paths.get(scalaResources).toFile)
  }

  /// Scala support

  /** Concatenate attributes by name in order */
  def standardArgs(exp:domain.Atomic) : String = {
    exp.attributes.map(att => att.instance + ":" + typeConverter(att.tpe)).mkString(",")
  }

  /** Concatenate attributes by name in order */
  def standardValArgs(exp:domain.Atomic) : String = {
    exp.attributes.map(att => "val " + att.instance + ":" + typeConverter(att.tpe)).mkString(",")
  }

  /**
    * Concatenate attributes by name in order, each with a trailing "_" as suffix. These are
    * useful for the parameter to a constructor
    */
  def constructorArgs(exp:domain.Atomic) : String = {
    exp.attributes.map(att => "val " + att.instance + "_ :" + typeConverter(att.tpe)).mkString(",")
  }

  /** Concatenate attributes by name in order with comma. */
  def standardParams(exp:domain.Atomic, suffix:String = "") : String = {
    exp.attributes.map(att => att.instance + suffix).mkString(",")
  }
}
