package example.expression.scalaVisitor  /*DI:LD:AD*/

import com.github.javaparser.ast.body.{BodyDeclaration, FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.j._
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait VisitorGenerator extends JavaGenerator with DataTypeSubclassGenerator with VisitorJavaBinaryMethod with OperationAsMethodGenerator with JavaBinaryMethod {
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

    //  binary methods for helper
    val decls:Seq[CompilationUnit] = if (flat.ops.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      helperClasses()
    } else {
      Seq.empty
    }

    decls ++ flat.types.map(tpe => generateExp(flat, tpe)) ++         // one class for each sub-type
      getModel.inChronologicalOrder                          // visitors are constructed in order
          .filter(m => m.ops.nonEmpty)
          .flatMap(m =>
              m.ops.map(op => operationGenerator(flat, op))) :+    // one class for each op
      generateBaseClass(flat) :+                              // abstract base class
      generateBase(flat)                                      // visitor gets its own class (overriding concept)
  }

  /**
    * Responsible for delegating to a new operation on the current context.
    *
    * This context is invariably defined by the existing method context which has a
    * parameter 'e' to satisfy the visit(Type e) method
    */
  override def delegateFixMe(exp:domain.Atomic, op:domain.Operation, params:Expression*) : Expression = {
    val opargs = params.mkString(",")
    Java(s"e.accept(new ${op.name.capitalize}($opargs))").expression[Expression]()
  }

  /** For Visitor Generator, same behavior as delegate. */
  override def identify(exp:domain.Atomic, op:domain.Operation, params:Expression*) : Expression = {
    delegateFixMe(exp, op, params : _*)
  }

  /** For visitor design solution, access through default 'e' parameter */
  override def subExpressions(exp:domain.Atomic) : Map[String,Expression] = {
    exp.attributes.map(att => att.name -> Java(s"e.get${att.name.capitalize}()").expression[Expression]()).toMap
  }

  /** Directly access local method, one per operation, with a parameter. */
  override def dispatch(expr:Expression, op:domain.Operation, params:Expression*) : Expression = {
    val args:String = params.mkString(",")
    Java(s"$expr.accept(new ${op.name.capitalize}($args))").expression()
  }

  /** Return designated Java type associated with type, or void if all else fails. */
  override def typeConverter(tpe:domain.TypeRep) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case domain.baseTypeRep => Java(s"${domain.baseTypeRep.name}").tpe()
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

  /**
    * For visitor, the base class defines the accept method used by all subclasses.
    * When BinaryMethods are present, also includes method to convert to Tree object
    */
  def generateBaseClass(model:domain.Model):CompilationUnit = {

    // Ignore passed in model in favor of just grabbing it on demand...
    val allOps = getModel.flatten().ops

    val binaryTreeInterface = if (allOps.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      Java(s"""public abstract tree.Tree ${domain.AsTree.name.toLowerCase}();""").classBodyDeclarations
    } else {
      Seq.empty
    }

    Java(s"""|package expression;
             |
             |public abstract class ${domain.baseTypeRep.name.capitalize} {
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
  override def methodGenerator(exp:domain.Atomic, op:domain.Operation): MethodDeclaration = {
    val retType = op.returnType match {
      case Some(tpe) => typeConverter(tpe)
      case _ => Java("void").tpe
    }

    Java(s"""|public $retType visit(${exp.name} e) {
             |  ${logic(exp, op).mkString("\n")}
             |}""".stripMargin).methodDeclarations().head
  }

  override def logicAsTree(exp:domain.Atomic) : Seq[MethodDeclaration] = {
    val atomicArgs = exp.attributes.map(att => att.name).mkString(",")

    // changes whether attributes can be access *directly* or whether they are accessed via getXXX*() method.
    val recursiveArgs = exp.attributes.map(att => att.name + s".${domain.AsTree.name.toLowerCase}()").mkString(",")

    val body:Seq[Statement] = exp match {
      case b:domain.Binary => {
        Java(s""" return new tree.Node(java.util.Arrays.asList($recursiveArgs), ${exp.hashCode()}); """).statements
      }
      case u:domain.Unary => {
        Java(s""" return new tree.Node(java.util.Arrays.asList($recursiveArgs), ${exp.hashCode()}); """).statements
      }
      case a:domain.Atomic => {
        Java(s""" return new tree.Leaf($atomicArgs);""").statements
      }
    }

    Java(
      s"""
         |public tree.Tree ${domain.AsTree.name.toLowerCase}() {
         |  ${body.mkString("\n")}
         |}""".stripMargin).methodDeclarations()
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
      case bm: domain.BinaryMethodTreeBase => true   // was BinaryMethod
      case _ => false
    }) {
      logicAsTree(exp)
    } else {
      Seq.empty
    }

    Java(s"""|package expression;
             |public class $name extends ${domain.baseTypeRep.name} {
             |
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
    val signatures = model.types.map(exp => methodGenerator(exp, op))

    // if operation has parameters then must add to visitor as well
    val atts:Seq[FieldDeclaration] = op.parameters.flatMap(p => Java(s"${typeConverter(p._2)} ${p._1};").fieldDeclarations())

    // only add constructor if visitor has a parameter
    val ctor = if (op.parameters.isEmpty) {
      ""
    } else {
      constructorFromOp(op)
    }

    // special case to be handled for BinaryMethods
    val tpe = op match {
      case bmb:domain.BinaryMethodTreeBase => Java(s"tree.Tree").tpe()
      case _ => typeConverter(op.returnType.get)
    }

    val s = Java(s"""|package expression;
                     |public class ${op.name.capitalize} extends Visitor<$tpe>{
                     |  ${ctor.toString}
                     |
                     |  ${atts.mkString("\n")}
                     |  ${signatures.mkString("\n")}
                     |}""".stripMargin)

    s.compilationUnit()
  }
}
