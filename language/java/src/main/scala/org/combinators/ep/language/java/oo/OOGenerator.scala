package org.combinators.ep.language.java.oo   /*DI:LD:AD*/

import com.github.javaparser.ast.body.MethodDeclaration
import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator._
import org.combinators.ep.language.java.{JavaBinaryMethod, JavaGenerator, JavaNameProvider, TestGenerator}
import org.combinators.templating.twirl.Java

/**
  * Object-orienetd generator
  *
  * @groupname approach Object-Oriented Approach to EP
  * @groupdesc approach Fundamental Helper methods for the oo approach to EP
  * @groupprio approach 0
  */
case class OOGenerator(evolution:Evolution, binaryMethod:JavaBinaryMethod) {
  val javaGen = JavaGenerator()
  import javaGen.{CompilationUnit,Expression,Type}
  import javaGen.{Context, Delta}

  /**
    * Generating a straight OO solution requires:
    * 1. A Class for every exp data type
    * 2. An Abstract Base class to be superclass of them all
    *
    * This method generates the proper code for the current model (retrieved via getModel).
    */
  def generatedCode():Seq[CompilationUnit] = {
    val flat = evolution.getModel.flatten

    // binary methods for helper
    val decls:Seq[CompilationUnit] = if (flat.hasBinaryMethod) {
      binaryMethod.generateHelperClasses().asInstanceOf[Seq[CompilationUnit]]
    } else {
      Seq.empty
    }

    decls ++ flat.typeCases.map(tpe => generateExp(tpe, flat.ops)) :+      // one class for each sub-type
      generateAbstractBaseClass(flat.ops)                              // base class $BASE
  }

  /** For straight design solution, directly access attributes by name. */
  def accessAttribute (exp:DataTypeCase, att:Attribute) : Expression = {
    Java(s"${JavaNameProvider.instanceNameOf(att.tpe)}").expression[Expression]
  }

  /** Handle self-case here. */
  def contextDispatch(source:Context, delta:Delta) : Expression = {
    if (delta.expr.isEmpty) {
      val op = delta.op.get
      val params = source.params.mkString(",")    // possible parameters to the operation
      Java(s"this.$op($params)").expression()
    } else {
      javaGen.contextDispatch(source, delta)
    }
  }

  /** Directly access local method, one per operation, with a parameter. */
  def dispatch(expr:Expression, op:Operation, params:Expression*) : Expression = {
    val args:String = params.mkString(",")
    Java(s"$expr.${JavaNameProvider.instanceNameOf(op)}($args)").expression()
  }

  // TODO: Consider removing this function
  /** Computer return type for given operation (or void). */
  def returnType(op:Operation): Type = javaGen.tpe(op.returnType)

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
    val params = javaGen.parameters(op)
    Java(s"""|public ${returnType(op)} ${JavaNameProvider.instanceNameOf(op)}($params) {
             |  ${javaGen.logic(exp, op).mkString("\n")}
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
             |public class $exp extends ${evolution.getModel.baseDataType} {
             |  ${javaGen.constructor(exp)}
             |  ${javaGen.fields(exp).mkString("\n")}
             |  ${methods.mkString("\n")}
             |}""".stripMargin).compilationUnit.asInstanceOf[CompilationUnit]
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
        s"${JavaNameProvider.instanceNameOf(op)}(${javaGen.parameters(op)});").methodDeclarations
    )

    Java(s"""|package oo;
             |public abstract class ${JavaNameProvider.conceptNameOf(evolution.getModel.baseDataType)} {  // replace evolution.domain.baseTypeRep.name with evolution.getModel.baseDataType
             |  ${signatures.mkString("\n")}
             |}""".stripMargin).compilationUnit.asInstanceOf[CompilationUnit]
  }
}
