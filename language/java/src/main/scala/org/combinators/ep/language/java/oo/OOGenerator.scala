package org.combinators.ep.language.java.oo   /*DI:LD:AD*/

import com.github.javaparser.ast.body.MethodDeclaration
import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import org.combinators.ep.language.java.{DataTypeSubclassGenerator, JavaBinaryMethod, JavaGenerator, OperationAsMethodGenerator}
import org.combinators.templating.twirl.Java

/**
  * Object-orienetd generator
  *
  * @groupname approach Object-Oriented Approach to EP
  * @groupdesc approach Fundamental Helper methods for the oo approach to EP
  * @groupprio approach 0
  */
trait OOGenerator
  extends JavaGenerator
    with OperationAsMethodGenerator
    with JavaBinaryMethod {

  val domain:BaseDomain with ModelDomain
  import domain._

  def getModel:domain.Model

  /**
    * Generating a straight OO solution requires:
    * 1. A Class for every exp data type
    * 2. An Abstract Base class to be superclass of them all
    *
    * This method generates the proper code for the current model (retrieved via getModel).
    */
  override def generatedCode():Seq[CompilationUnit] = {
    val flat = getModel.flatten()

    // binary methods for helper
    val decls:Seq[CompilationUnit] = if (flat.hasBinaryMethod()) {
      helperClasses()
    } else {
      Seq.empty
    }

    decls ++ flat.types.map(tpe => generateExp(tpe, flat.ops)) :+      // one class for each sub-type
      generateAbstractBaseClass(flat.ops)                              // base class $BASE
  }

  /** For straight design solution, directly access attributes by name. */
  override def expression (exp:DataType, att:Attribute) : Expression = {
    Java(s"${att.instance}").expression[Expression]
  }

  /** Handle self-case here. */
  override def contextDispatch(source:Context, delta:Delta) : Expression = {
    if (delta.expr.isEmpty) {
      val op = delta.op.get.instance
      val params = source.params.mkString(",")    // possible parameters to the operation
      Java(s"this.$op($params)").expression()
    } else {
      super.contextDispatch(source, delta)
    }
  }

  /** Directly access local method, one per operation, with a parameter. */
  override def dispatch(expr:Expression, op:Operation, params:Expression*) : Expression = {
    val args:String = params.mkString(",")
    Java(s"$expr.${op.instance}($args)").expression()
  }

  /** Computer return type for given operation (or void). */
  def returnType(op:Operation): Type = {
    op.returnType match {
      case Some(tpe) => typeConverter(tpe)
      case _ => Java("void").tpe
    }
  }

  /**
    * Operations are implemented as methods in the Base and sub-type classes.
    *
    * @param exp  Desired Data type
    * @param op   Desired Operation
    * @return Method that contains the logic for applying given operation to this data type.
    *
    * @group api
    */
  def methodGenerator(exp:DataType, op:Operation): MethodDeclaration = {
    val params = parameters(op)
    Java(s"""|public ${returnType(op)} ${op.instance}($params) {
             |  ${logic(exp, op).mkString("\n")}
             |}""".stripMargin).methodDeclarations.head
  }

  /**
    * Generate the full class for the given expression sub-type for the expected
    * operations.
    *
    * @param exp   desired data type to construct class for
    * @param ops   all operations required in the system
    * @group api
    */
  def generateExp(exp:DataType, ops:Seq[Operation]) : CompilationUnit = {
    val methods = ops.map(op => methodGenerator(exp, op))

    Java(s"""|package oo;
             |public class $exp extends ${domain.baseTypeRep.name} {
             |  ${constructor(exp)}
             |  ${fields(exp).mkString("\n")}
             |  ${methods.mkString("\n")}
             |}""".stripMargin).compilationUnit
  }

  /**
    * Generate the abstract base class, with all operations from flattened history.
    *
    * The name of the generated class is drawn from the domain's baseTypeRep.
    *
    * @param ops   All desired operations
    */
  def generateAbstractBaseClass(ops:Seq[Operation]): CompilationUnit = {
    val signatures = ops.flatMap(op =>
       Java(s"public abstract ${returnType(op)} " +
        s"${op.instance}(${parameters(op)});").methodDeclarations
    )

    Java(s"""|package oo;
             |public abstract class ${domain.baseTypeRep.name} {
             |  ${signatures.mkString("\n")}
             |}""".stripMargin).compilationUnit
  }
}
