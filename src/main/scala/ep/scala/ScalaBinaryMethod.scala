package ep.scala    /*DI:LD:AI*/

import java.io.File
import java.nio.file.{Path, Paths}
import ep.domain.{BaseDomain, ModelDomain}
import scala.meta._

trait ScalaBinaryMethod {
  val domain:BaseDomain with ModelDomain
  import domain._

  /** Specially required files are placed in this area. */
  val scalaResources:String = Seq("src", "main", "resources", "scala-code").mkString(File.separator)

  /** Taken from scala meta web page. */
  def loadSource(entry:String*) : ScalaMainWithPath = {
    val path:Path = java.nio.file.Paths.get(scalaResources, entry: _*)
    val bytes = java.nio.file.Files.readAllBytes(path)
    val text = new String(bytes, "UTF-8")
    val input = Input.VirtualFile(path.toString, text)

    ScalaMainWithPath(input.parse[Source].get, Paths.get(entry.head, entry.tail : _*))
  }

  /**
    *
    * Helpful snippet to get all regular files below a given directory, using
    * the specified header as the relative path to those files
    */
  def getRecursiveListOfFiles(dir: File, header:String*): Seq[ScalaMainWithPath] = {
    val these:Seq[File] = dir.listFiles
    if (these == null || these.isEmpty) {
      Seq.empty
    } else {
      val sources: Seq[ScalaMainWithPath] = these.filterNot(f => f.isDirectory).map(f => loadSource(header :+ f.getName: _*))

      sources ++ these.filter(_.isDirectory).flatMap(f => getRecursiveListOfFiles(f, header :+ f.getName: _*))
    }
  }

  /**
    * Binary methods creates helper classes in package 'tree'. Completes description
    * of tree-based structure to represent the expression, using unique values for each
    * expression sub-type.
    *
    * @return
    */
  def helperClasses():Seq[ScalaWithPath] = {
    getRecursiveListOfFiles(Paths.get(scalaResources).toFile)
  }

  /**
    * Compute parameter "Type name" comma-separated list from operation. Be sure to convert BaseType into op.name!
    *
    * @param op               operation under consideration
    * @param typeConverter    existing typeconverter which we need for other types besides baseTypeRep
    * @return                 return new parameter type with op interface used in place of baseTypeRep
    */
  def binaryMethodParameters(op:domain.Operation, typeConverter:(domain.TypeRep) => Type) : String = {
    op.parameters.map(param => {
      // use operation name for binary method
      val realType = param.tpe match {
        case domain.baseTypeRep => op.name.capitalize
        case _ => typeConverter(param.tpe)
      }

      realType.toString + " " + param.name
    }).mkString(",")
  }

  def logicAsTree(exp:domain.Atomic) : Seq[Stat] = {
    val args = exp.attributes.map(att => att.name).mkString(",")
          Scala(
            s"""
               |def ${domain.AsTree.name.toLowerCase}() : tree.Tree =  {
               |  return asTree.${exp.name.toLowerCase}($args).${domain.AsTree.name.toLowerCase}();
               |}""".stripMargin).statements
  }

  /** Interesting shift needed for visitor. */
  def visitorLogicAsTree(exp:domain.Atomic) : Seq[Stat] = {
    val atomicArgs = exp.attributes.map(att => att.name).mkString(",")

    // changes whether attributes can be access *directly* or whether they are accessed via getXXX*() method.
    val recursiveArgs = exp.attributes.map(att => att.name + s".${AsTree.name.toLowerCase}()").mkString(",")

    val body:Seq[Stat] = exp match {
      case b:Binary => {
        Scala(s""" new tree.Node(Seq($recursiveArgs), ${exp.hashCode()}) """).statements
      }
      case u:Unary => {
        Scala(s""" new tree.Node(Seq($recursiveArgs), ${exp.hashCode()}) """).statements
      }
      case a:Atomic => {
        Scala(s""" new tree.Leaf($atomicArgs);""").statements
      }
    }

    Scala(
      s"""
         |def ${domain.AsTree.name.toLowerCase}() : tree.Tree  = {
         |  ${body.mkString("\n")}
         |}""".stripMargin).statements
  }
}
