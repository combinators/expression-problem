package example.expression.j      /*DI:LD:AI*/

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.body.{BodyDeclaration, MethodDeclaration}
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.generator.BinaryMethod
import org.combinators.templating.twirl.Java

trait JavaBinaryMethod extends BinaryMethod {
  val domain:BaseDomain with ModelDomain
  import domain._


  /**
    * Binary methods creates helper classes in package 'tree'. Completes description
    * of tree-based structure to represent the expression, using unique values for each
    * expression sub-type.
    *
    * @return
    */
  def helperClasses():Seq[CompilationUnit] = {
    Seq(
      example.expression.java.Tree.render(Java("tree").name()).compilationUnit(),
      example.expression.java.Node.render(Java("tree").name()).compilationUnit(),
      example.expression.java.Leaf.render(Java("tree").name()).compilationUnit()
    )
  }

  /**
    * Compute parameter "Type name" comma-separated list from operation. Be sure to convert BaseType into op.name!
    *
    * @param op               operation under consideration
    * @param typeConverter    existing typeconverter which we need for other types besides baseTypeRep
    * @return                 return new parameter type with op interface used in place of baseTypeRep
    */
  def binaryMethodParameters(op:domain.Operation, typeConverter:(domain.TypeRep,Option[Type]) => Type) : String = {
    op.parameters.map(tuple => {
      val name: String = tuple._1
      val tpe: domain.TypeRep = tuple._2

      // use operation name for binary method
      val realType = tpe match {
        case domain.baseTypeRep => op.name.capitalize
        case _ => typeConverter(tpe, Option.empty)
      }

      realType.toString + " " + name
    }).mkString(",")
  }

  def logicAsTree(exp:domain.Atomic) : Seq[MethodDeclaration] = {
    val args = exp.attributes.map(att => att.name).mkString(",")
          Java(
            s"""
               |public tree.Tree ${domain.AsTree.name.toLowerCase}() {
               |  return asTree.${exp.name.toLowerCase}($args).${domain.AsTree.name.toLowerCase}();
               |}""".stripMargin).methodDeclarations()
  }

  /** Interesting shift needed for visitor. */
  def visitorLogicAsTree(exp:domain.Atomic) : Seq[MethodDeclaration] = {
    val atomicArgs = exp.attributes.map(att => att.name).mkString(",")

    // changes whether attributes can be access *directly* or whether they are accessed via getXXX*() method.
    val recursiveArgs = exp.attributes.map(att => att.name + s".${AsTree.name.toLowerCase}()").mkString(",")

    val body:Seq[Statement] = exp match {
      case b:Binary => {
        Java(s""" return new tree.Node(java.util.Arrays.asList($recursiveArgs), ${exp.hashCode()}); """).statements
      }
      case u:Unary => {
        Java(s""" return new tree.Node(java.util.Arrays.asList($recursiveArgs), ${exp.hashCode()}); """).statements
      }
      case a:Atomic => {
        Java(s""" return new tree.Leaf($atomicArgs);""").statements
      }
    }

    Java(
      s"""
         |public tree.Tree ${domain.AsTree.name.toLowerCase}() {
         |  ${body.mkString("\n")}
         |}""".stripMargin).methodDeclarations()
  }
}
