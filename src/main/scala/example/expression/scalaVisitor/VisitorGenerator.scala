package example.expression.scalaVisitor  /*DI:LD:AD*/

import com.github.javaparser.ast.body.{BodyDeclaration, FieldDeclaration, MethodDeclaration}
import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.j._
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait VisitorGenerator extends AbstractGenerator with DataTypeSubclassGenerator with VisitorJavaBinaryMethod with OperationAsMethodGenerator with Producer with JavaBinaryMethod {
  val domain:BaseDomain with ModelDomain

  /**
    * Generating a visitor solution requires:
    *
    * 1. A Class for every data type
    * 2. A Class for every operation
    * 3. Abstract Base class and visitor class
    * @return
    */
  def generatedCode():Seq[CompilationUnit] = {
    val flat = getModel.flatten()
    flat.types.map(tpe => generateExp(flat, tpe)) ++         // one class for each sub-type
      getModel.inChronologicalOrder                          // visitors are constructed in order
          .filter(m => m.ops.nonEmpty)
          .flatMap(m =>
              m.ops.map(op => operationGenerator(flat, op))) :+    // one class for each op
      generateBaseClass(flat) :+                              // abstract base class
      generateBase(flat)                                      // visitor gets its own class (overriding concept)
  }

  /** For visitor design solution, access through default 'e' parameter */
  override def subExpressions(exp:domain.Atomic) : Map[String,Expression] = {
    exp.attributes.map(att => att.name -> Java(s"e.get${att.name.capitalize}()").expression[Expression]()).toMap
  }

  /** Directly access local method, one per operation, with a parameter. */
  override def dispatch(expr:Expression, op:domain.Operation, params:Expression*) : Expression = {
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

    Java (s"""
              |package expression;
              |/*
              | * A concrete visitor describes a concrete operation on expressions. There is one visit
              | * method per type in the class hierarchy.
              | */
              |public abstract class Visitor<R> {
              |  $signatures
              |}""".stripMargin).compilationUnit()
  }

  /** For visitor, the base class defines the accept method used by all subclasses. */
  def generateBaseClass(model:domain.Model):CompilationUnit = {

    // Ignore passed in model in favor of just grabbing it on demand...
    val allOps = getModel.flatten().ops

    // If BinaryMethodTreeBase is defined, then need Astree declarations...
    val decls = if (allOps.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      declarations
    } else {
      Seq.empty
    }

    val binaryTreeInterface = if (allOps.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      Java(s"""public abstract Tree ${domain.AsTree.name.toLowerCase}();""").classBodyDeclarations
    } else {
      Seq.empty
    }

    Java(s"""|package expression;
             |
             |public abstract class ${domain.baseTypeRep.name.capitalize} {
             |    ${decls.mkString("\n")}
             |    ${binaryTreeInterface.mkString("\n")}
             |    public abstract <R> R accept(Visitor<R> v);
             |}
             |""".stripMargin).compilationUnit()
  }

  /**
    * Operations are implemented as methods in the Base and sub-type classes.
    *
    * Note that BinaryMethodBase is handled separately
    * As is BinaryMethods
    */
  override def methodGenerator(exp:domain.Atomic)(op:domain.Operation): MethodDeclaration = {
    val retType = op.returnType match {
      case Some(tpe) => typeConverter(tpe)
      case _ => Java("void").tpe
    }

    op match {
      case bmb:domain.BinaryMethodTreeBase =>  Java(s"""|public ${domain.baseTypeRep.name}.Tree visit(${exp.name} e) {
                                                        |  return e.${domain.AsTree.name.toLowerCase}();
                                                        |}""".stripMargin).methodDeclarations().head
//      case bm:domain.BinaryMethod => Java(s"""|public $retType visit(${exp.name} e) {
//                                              |  ${logic(exp)(op).mkString("\n")}
//                                              |}""".stripMargin).methodDeclarations().head
      case _ => Java(s"""|public $retType visit(${exp.name} e) {
                         |  ${logic(exp)(op).mkString("\n")}
                         |}""".stripMargin).methodDeclarations().head
    }

  }


  /** Generate the full class for the given expression sub-type. */
  override def generateExp(model:domain.Model, exp:domain.Atomic) : CompilationUnit = {
    val name = exp.toString

    val visitor:MethodDeclaration = Java (s"""|public <R> R accept(Visitor<R> v) {
                                              |   return v.visit(this);
                                              |}""".stripMargin).methodDeclarations().head

    // Ignore passed in model in favor of just grabbing it on demand...
    val allOps = getModel.flatten().ops
    val helpers:Seq[BodyDeclaration[_]] = if (allOps.exists {
      case bm: domain.BinaryMethodTreeBase => true   // was BInaryMethod
      case _ => false
    }) {
      visitorLogicAsTree(exp)
    } else {
      Seq.empty
    }

    val definedSubtypes:Seq[BodyDeclaration[_]] = if (allOps.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      definedDataSubTypes("", Seq(exp))
    } else {
      Seq.empty
    }

    Java(s"""|package expression;
             |public class $name extends Exp {
             |  ${definedSubtypes.mkString("\n")}
             |  ${constructor(exp)}
             |  ${helpers.mkString("\n")}
             |  ${fields(exp).mkString("\n")}
             |  ${getters(exp).mkString("\n")}
             |  ${visitor.toString()}
             |}""".stripMargin).compilationUnit()
  }

  /**
    * Brings in classes for each operation. These can only be completed with the implementations.
    *
    * Must handle BinaryMethod (Equals) and BinaryMethodBase (Astree) specially.
    */
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

    val tpe = op match {
      case bmb:domain.BinaryMethodTreeBase => Java(s"${domain.baseTypeRep.name}.Tree").tpe()
      case _ => typeConverter(op.returnType.get)
    }

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
