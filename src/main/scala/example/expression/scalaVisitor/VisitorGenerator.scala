package example.expression.scalaVisitor

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.body.{FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.j._
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait VisitorGenerator extends AbstractGenerator with JavaGenerator with DataTypeSubclassGenerator with OperationAsMethodGenerator with BinaryMethod {
  val domain:BaseDomain with ModelDomain

  /**
    * For visitor, must flatten entire hierarchy
    */
  override def apply(model:domain.Model):domain.Model = model.flat()

  /**
    * Generating a visitor solution requires:
    *
    * 1. A Class for every data type
    * 2. A Class for every operation
    * 3. Abstract Base class and visitor class
    * @param model
    * @return
    */
  override def generatedCode(model:domain.Model):Seq[CompilationUnit] = {
    model.types.map(tpe => generateExp(model, tpe)) ++         // one class for each sub-type
      model.ops.map(op => operationGenerator(model, op)) :+    // one class for each op
      generateBaseClass() :+                                   // abstract base class
      generateBase(model)                                      // visitor gets its own class (overriding concept)
  }

  /** For visitor design solution, access through default 'e' parameter */
  override def subExpressions(exp:domain.Atomic) : Map[String,Expression] = {
    exp.attributes.map(att => att.name -> Java(s"e.get${att.name.capitalize}()").expression[Expression]()).toMap
  }

  override def getJavaClass : Expression = {
    Java(s"e.getClass()").expression[Expression]()
  }

  /** Directly access local method, one per operation, with a parameter. */
  override def recurseOn(expr:Expression, op:domain.Operation, params:Expression*) : Expression = {
    val args:String = params.mkString(",")
    Java(s"""$expr.accept(new ${op.name.capitalize}($args))""").expression()
  }


  /** Return designated Java type associated with type, or void if all else fails. */
  override def typeConverter(tpe:domain.TypeRep, covariantReplacement:Option[Type] = None) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case domain.baseTypeRep => Java("Exp").tpe()
    }
  }

  /** Return Visitor class, which contains a visit method for each available sub-type. */
  override def generateBase(model:domain.Model): CompilationUnit = {
    val signatures = model.types
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

  /** For visitor, the base class defines the accept method used by all subclasses. */
  def generateBaseClass():CompilationUnit = {
    Java(s"""|package expression;
             |
             |public abstract class Exp {
             |    public abstract <R> R accept(Visitor<R> v);
             |}
             |""".stripMargin).compilationUnit()
  }

  /** Operations are implemented as methods in the Base and sub-type classes. */
  override def methodGenerator(exp:domain.Atomic)(op:domain.Operation): MethodDeclaration = {
    val retType = op.returnType match {
      case Some(tpe) => typeConverter(tpe)
      case _ => Java("void").tpe
    }

    Java(s"""|public $retType visit(${exp.name} e) {
             |  ${logic(exp)(op).mkString("\n")}
             |}""".stripMargin).methodDeclarations().head
  }


  /** Generate the full class for the given expression sub-type. */
  override def generateExp(model:domain.Model, exp:domain.Atomic) : CompilationUnit = {
    val name = exp.toString

    val visitor:MethodDeclaration = Java (s"""|public <R> R accept(Visitor<R> v) {
                                              |   return v.visit(this);
                                              |}""".stripMargin).methodDeclarations().head

    Java(s"""|package expression;
             |public class $name extends Exp {
             |
             |  ${constructor(exp)}
             |
             |  ${fields(exp).mkString("\n")}
             |  ${getters(exp).mkString("\n")}
             |  ${visitor.toString()}
             |}""".stripMargin).compilationUnit()
  }

  /** Brings in classes for each operation. These can only be completed with the implementations. */
  def operationGenerator(model:domain.Model, op:domain.Operation): CompilationUnit = {
    val signatures = model.types.map(exp => methodGenerator(exp)(op)).mkString("\n")

    // if operation has parameters then must add to visitor as well
    val atts:Seq[FieldDeclaration] = op.parameters.flatMap(p => Java(s"${typeConverter(p._2)} ${p._1};").fieldDeclarations())

    // only add constructor if visitor has a parameter
    val ctor = if (op.parameters.isEmpty) {
      ""
    } else {
      constructorFromOp(op)
    }

    val tpe = typeConverter(op.returnType.get)
    val s = Java(s"""|package expression;
                     |public class ${op.name.capitalize} extends Visitor<$tpe>{
                     |  ${ctor.toString}
                     |
                     |  ${atts.mkString("\n")}
                     |  $signatures
                     |}""".stripMargin)

    s.compilationUnit()
  }
}
