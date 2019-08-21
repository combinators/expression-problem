package org.combinators.ep.language.java.oo   /*DI:LD:AD*/

import com.github.javaparser.ast.body.MethodDeclaration
import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator._
import org.combinators.ep.language.java.{DataTypeSubclassGenerator, JavaBinaryMethod, DomainIndependentJavaGenerator, OperationAsMethodGenerator}
import org.combinators.templating.twirl.Java

/**
  * Object-orienetd generator
  *
  * @groupname approach Object-Oriented Approach to EP
  * @groupdesc approach Fundamental Helper methods for the oo approach to EP
  * @groupprio approach 0
  */
class OOGenerator(val evolution:Evolution, val binaryMethod:JavaBinaryMethod, val javaGen:DomainIndependentJavaGenerator)
  extends DomainDependentGenerator (javaGen)
    with OperationAsMethodGenerator  {

  import javaGen._

  /**
    * Generating a straight OO solution requires:
    * 1. A Class for every exp data type
    * 2. An Abstract Base class to be superclass of them all
    *
    * This method generates the proper code for the current model (retrieved via getModel).
    */
  override def generatedCode():Seq[CompilationUnit] = {
    val flat = evolution.getModel.flatten

    // binary methods for helper
    val decls:Seq[CompilationUnit] = if (flat.hasBinaryMethod) {
      binaryMethod.generateHelperClasses()
    } else {
      Seq.empty
    }

    decls ++ flat.typeCases.map(tpe => generateExp(tpe, flat.ops)) :+      // one class for each sub-type
      generateAbstractBaseClass(flat.ops)                              // base class $BASE
  }

  /** For straight design solution, directly access attributes by name. */
  override def accessAttribute (exp:DataTypeCase, att:Attribute) : Expression = {
    Java(s"${names.instanceNameOf(att.tpe)}").expression[Expression]
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

  // TODO: Consider removing this function
  /** Computer return type for given operation (or void). */
  def returnType(op:Operation): Type = typeConverter(op.returnType)

  /**
    * Operations are implemented as methods in the Base and sub-type classes.
    *
    * @param exp  Desired Data type
    * @param op   Desired Operation
    * @return Method that contains the logic for applying given operation to this data type.
    *
    * @group api
    */
  def methodGenerator(exp:DataTypeCase, op:Operation): MethodDeclaration = {
    val params = parameters(op)
    Java(s"""|public ${returnType(op)} ${names.instanceNameOf(op)}($params) {
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
  def generateExp(exp:DataTypeCase, ops:Seq[Operation]) : CompilationUnit = {
    val methods = ops.map(op => methodGenerator(exp, op))

    Java(s"""|package oo;
             |public class $exp extends ${evolution.domain.baseTypeRep.name} {
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
             |public abstract class ${evolution.domain.baseTypeRep.name} {
             |  ${signatures.mkString("\n")}
             |}""".stripMargin).compilationUnit
  }
}
