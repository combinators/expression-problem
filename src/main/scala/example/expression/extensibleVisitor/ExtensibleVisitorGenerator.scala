package example.expression.extensibleVisitor    /*DI:LD:AD*/

import com.github.javaparser.ast.body.TypeDeclaration
import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.scalaVisitor.{VisitorGenerator, VisitorJavaBinaryMethod}
import org.combinators.templating.twirl.Java

/**
  * Synthesizing OO and Functional Design to promote Reuse
  * Shriram Krishnamurthi, Matthias Felleisen, Daniel Friedman
  * https://dl.acm.org/citation.cfm?id=679709
  */
trait ExtensibleVisitorGenerator extends VisitorGenerator with VisitorJavaBinaryMethod {
  val domain:BaseDomain with ModelDomain

  /**
    * Generating a visitor solution requires:
    *
    * 1. A Class for every data type
    * 2. A Class for every operation
    * 3. Abstract Base class and visitor class
    * @return
    */
  override def generatedCode():Seq[CompilationUnit] = {
    val flat = getModel.flatten()
    flat.types.map(tpe => generateExp(flat, tpe)) ++         // one class for each sub-type
      flat.ops.map(op => operationGenerator(flat, op)) :+    // one class for each op
      generateBaseClass(flat) :+                             // abstract base class
      generateBase(flat)                                     // visitor gets its own class (overriding concept)
  }

  /** For visitor design solution, access through default 'e' parameter */
  override def subExpressions(exp:domain.Atomic) : Map[String,Expression] = {
    exp.attributes.map(att => att.name -> Java(s"e.get${att.name.capitalize}()").expression[Expression]()).toMap
  }

  /** Add virtual type generator. */
  def addVirtualConstructor(mainType:TypeDeclaration[_], op:domain.Operation) : Unit = {
    val virtualConstructor = Java(
             s"""|${op.name.capitalize} make${op.name.capitalize} (${parameters(op)}) {
                 |  return new ${op.name.capitalize} (${arguments(op)});
                 |}""".stripMargin).methodDeclarations().head

    mainType.addMember(virtualConstructor)
  }

  /** Directly access local method, one per operation, with a parameter. */
  override def dispatch(expr:Expression, op:domain.Operation, params:Expression*) : Expression = {
    val args:String = params.mkString(",")
    Java(s"""$expr.accept(make${op.name.capitalize}($args))""").expression()
  }


  /** Return designated Java type associated with type, or void if all else fails. */
  override def typeConverter(tpe:domain.TypeRep, covariantReplacement:Option[Type] = None) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case domain.baseTypeRep => Java("Exp").tpe()
    }
  }

  /** Return Visitor class, which contains a visit method for each available sub-type in past. */
  override def generateBase(model:domain.Model): CompilationUnit = {
    val signatures = model.pastDataTypes
      .map(exp => s"public abstract R visit(${exp.name} exp);").mkString("\n")

    Java (s"""|package expression;
              |/*
              | * A concrete visitor describes a concrete operation on expressions. There is one visit
              | * method per type in the class hierarchy.
              | */
              |public abstract class Visitor<R> {
              |  $signatures
              |}""".stripMargin).compilationUnit()
  }

  /** Brings in classes for each operation. These can only be completed with the implementations. */
  override def operationGenerator(model:domain.Model, op:domain.Operation): CompilationUnit = {
    val regularVisitor:CompilationUnit = super.operationGenerator(model, op)

    val mainType:TypeDeclaration[_] = regularVisitor.getType(0)
    addVirtualConstructor(mainType, op)
    regularVisitor
  }
}
