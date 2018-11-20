package example.expression.scala    /*DI:LD:AI*/

import java.nio.file.Path

import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.generator.BinaryMethod

import scala.meta._

trait ScalaBinaryMethod extends BinaryMethod {
  val domain:BaseDomain with ModelDomain
  import domain._

  /** Taken from scala meta web page. */
  def loadToSource(entry:String) : ScalaWithPath = {
    val path:Path = java.nio.file.Paths.get("src", "main", "scala", "tree", entry)
    val bytes = java.nio.file.Files.readAllBytes(path)
    val text = new String(bytes, "UTF-8")
    val input = Input.VirtualFile(path.toString, text)

    ScalaWithPath(input.parse[Source].get, java.nio.file.Paths.get("tree", entry))
  }

  /**
    * Binary methods creates helper classes in package 'tree'. Completes description
    * of tree-based structure to represent the expression, using unique values for each
    * expression sub-type.
    *
    * @return
    */
  def helperClasses():Seq[ScalaWithPath] = {
    Seq(
      loadToSource("Leaf.scala"),
      loadToSource("Node.scala"),
      loadToSource("Tree.scala")
    )
  }

  /**
    * Compute parameter "Type name" comma-separated list from operation. Be sure to convert BaseType into op.name!
    *
    * @param op               operation under consideration
    * @param typeConverter    existing typeconverter which we need for other types besides baseTypeRep
    * @return                 return new parameter type with op interface used in place of baseTypeRep
    */
  def binaryMethodParameters(op:domain.Operation, typeConverter:(domain.TypeRep) => Type) : String = {
    op.parameters.map(tuple => {
      val name: String = tuple._1
      val tpe: domain.TypeRep = tuple._2

      // use operation name for binary method
      val realType = tpe match {
        case domain.baseTypeRep => op.name.capitalize
        case _ => typeConverter(tpe)
      }

      realType.toString + " " + name
    }).mkString(",")
  }

  def logicAsTree(exp:domain.Atomic) : Seq[Stat] = {
    val args = exp.attributes.map(att => att.name).mkString(",")
          Scala(
            s"""
               |def ${domain.AsTree.name.toLowerCase}() : tree.Tree =  {
               |  return asTree.${exp.name.toLowerCase}($args).${domain.AsTree.name.toLowerCase}();
               |}""".stripMargin).statements()
  }

  /** Interesting shift needed for visitor. */
  def visitorLogicAsTree(exp:domain.Atomic) : Seq[Stat] = {
    val atomicArgs = exp.attributes.map(att => att.name).mkString(",")

    // changes whether attributes can be access *directly* or whether they are accessed via getXXX*() method.
    val recursiveArgs = exp.attributes.map(att => att.name + s".${AsTree.name.toLowerCase}()").mkString(",")

    val body:Seq[Stat] = exp match {
      case b:Binary => {
        Scala(s""" new tree.Node(Seq($recursiveArgs), ${exp.hashCode()}) """).statements()
      }
      case u:Unary => {
        Scala(s""" new tree.Node(Seq($recursiveArgs), ${exp.hashCode()}) """).statements()
      }
      case a:Atomic => {
        Scala(s""" new tree.Leaf($atomicArgs);""").statements()
      }
    }

    Scala(
      s"""
         |def ${domain.AsTree.name.toLowerCase}() : tree.Tree  = {
         |  ${body.mkString("\n")}
         |}""".stripMargin).statements()
  }
}
