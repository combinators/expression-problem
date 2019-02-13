package org.combinators.ep.language.java

/*DI:LD:AI*/

import com.github.javaparser.JavaParser
import com.github.javaparser.ast.{CompilationUnit, PackageDeclaration}
import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.body.MethodDeclaration
import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import org.combinators.templating.twirl.Java

trait JavaBinaryMethod {
  val domain:BaseDomain with ModelDomain

  /**
    * Binary methods creates helper classes in package 'tree'. Completes description
    * of tree-based structure to represent the expression, using unique values for each
    * expression sub-type.
    *
    * @return
    */
  def helperClasses(): Seq[CompilationUnit] = {
    val classes =
      Seq(
        JavaParser.parse(getClass.getResourceAsStream("/java-code/Tree.java")),
        JavaParser.parse(getClass.getResourceAsStream("/java-code/Node.java")),
        JavaParser.parse(getClass.getResourceAsStream("/java-code/Leaf.java"))
      )
    classes.foreach(compilationUnit => compilationUnit.setPackageDeclaration("tree"))
    classes
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
        case domain.baseTypeRep => op.concept
        case _ => typeConverter(param.tpe)
      }

      realType.toString + " " + param.name
    }).mkString(",")
  }

  /**
    * Return a tree.Tree object representing the given sub-type expression.
    *
    * @param exp
    * @return
    */
  def logicAsTree(exp:domain.DataType) : Seq[MethodDeclaration] = {
    val args = exp.attributes.map(att => att.instance).mkString(",")
          Java(
            s"""
               |public tree.Tree ${domain.AsTree.instance}() {
               |  return asTree.${exp.instance}($args).${domain.AsTree.instance}();
               |}""".stripMargin).methodDeclarations()
  }
}
