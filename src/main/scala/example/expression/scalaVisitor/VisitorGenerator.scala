package example.expression.scalaVisitor

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.{FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.Domain
import example.expression.j.AbstractGenerator
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait VisitorGenerator extends AbstractGenerator {
  val domain:Domain
  import domain._

//  /** Request given operation on the Java identifier. */
//  def oper(expVar:String, op:Operation): Expression = {
//    Java(s"$expVar.accept(new ${op.name.capitalize}())").expression()
//  }

  /** For straight design solution, directly access attributes by name. */
  override def subExpressions(exp:expressions.Exp) : Seq[Expression] = {
    exp.attributes.map(a => Java(s"e.get${a.name.capitalize}()").expression[Expression]())

//    exp match {
//      case Lit => Seq(Java("e.getValue()").expression[Expression]())
//      case e: expressions.UnaryExp => Seq(Java(s"e.get${exp.attributes(0).name.capitalize}()").expression[Expression]())
//      case b: expressions.BinaryExp => Seq(Java("e.getLeft()").expression[Expression](), Java("e.getRight()").expression[Expression]())
//    }
  }

  /** Directly access local method, one per operation. */
  override def recurseOn(expr:Expression, op:Operation) : Expression = {
    Java(s"""$expr.accept(new ${op.name.capitalize}())""").expression()
  }

  /** Return designated Java type associated with type, or void if all else fails. */
  def typeGenerator(tpe:types.Types) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case types.Exp => Java("Exp").tpe()
      case _ => Java ("void").tpe()  // reasonable stop
    }
  }

  /** Return Visitor class, which contains a visit method for each available sub-type. */
  def generateBase(model:Model): CompilationUnit = {
      val signatures = model.types
        .map(exp => s"public abstract R visit(${exp.name} exp);").mkString("\n")

      Java (s"""|package expression;
                |/*
                | * A concrete visitor describes a concrete operation on expressions. There is one visit
                | * method per type in the class hierarchy.
                | */
                |public abstract class Visitor<R> {
                |
                |$signatures
                |}""".stripMargin).compilationUnit()
  }

  def generateBaseClass():CompilationUnit = {
    Java(s"""|package expression;
               |
               |public abstract class Exp {
               |    public abstract <R> R accept(Visitor<R> v);
               |}
               |""".stripMargin).compilationUnit()
  }

  /** Each sub-type has an accept method as required by visitor pattern */
  def visitorMethodGenerator(): MethodDeclaration = {
       Java (s"""
             |public <R> R accept(Visitor<R> v) {
             |   return v.visit(this);
             |}""".stripMargin).methodDeclarations().head
  }

  /** Operations are implemented as methods in the Base and sub-type classes. */
  def methodGenerator(exp:expressions.Exp)(op:Operation): MethodDeclaration = {
    val retType = op.returnType match {
      case Some(tpe) => typeGenerator(tpe)
      case _ => Java("void").tpe
    }

    Java(s"""|public $retType visit(${exp.name} e) {
             |  ${methodBodyGenerator(exp)(op).mkString("\n")}
             |}""".stripMargin).methodDeclarations().head
  }

  /** Brings in classes for each operation. These can only be completed with the implementations. */
  def operationGenerator(model:Model, op:Operation): CompilationUnit = {

    val tpe = typeGenerator(op.returnType.get)
//    val signatures = model.types.map(exp => {
//      val stmts:Seq[Statement] = methodBodyGenerator(exp)(op)     // grab from abstract generator
//      s"public $tpe visit(${exp.name} e) { ${stmts.mkString("\n")} }"
//    }).mkString("\n")
  val signatures = model.types.map(exp => methodGenerator(exp)(op)).mkString("\n")

    val s = Java(s"""|package expression;
                     |public class ${op.name.capitalize} extends Visitor<$tpe>{
                     |  $signatures
                     |}""".stripMargin)

    s.compilationUnit()
  }

  def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = {
    throw new scala.NotImplementedError(s"""Operation "${op.name}" does not handle case for sub-type "${exp.name}" """)
  }

//  /** Return sample JUnit test cases. */
//  def testGenerator(): Seq[MethodDeclaration] = Seq.empty
//
//  /** Convert a test instance into a Java Expression for instantiating that instance. */
//  def convert(inst:instances.ExpInst) : Expression = {
//    val name = inst.e.name
//
//    inst match {
//      case lit:LitInst => Java(s"new $name(${lit.i.get.toString})").expression()
//      case ui:instances.UnaryExpInst =>
//        Java(s"new $name(${convert(ui.exp)})").expression()
//      case bi:instances.BinaryExpInst =>
//        Java(s"new $name(${convert(bi.left)}, ${convert(bi.right)})").expression()
//      case _ =>  Java(s""" "unknown $name" """).expression()
//    }
//  }
//
//  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
//  def generateSuite(): CompilationUnit = {
//    val methods:Seq[MethodDeclaration] = testGenerator()
//
//    var num:Int = 0
//    val unitTests:Seq[MethodDeclaration] = methods.filter(md => md.getBody.isPresent).map(md => {
//      num = num + 1
//      Java (s"""public void test$num()  ${md.getBody.get.toString} """).methodDeclarations().head
//    })
//
//    Java(s"""|package expression;
//             |import junit.framework.TestCase;
//             |public class TestSuite extends TestCase {
//             |    ${unitTests.mkString("\n")}
//             |}""".stripMargin).compilationUnit()
//  }

  /** Generate the full class for the given expression sub-type. */
  def generateExp(domain:Model, e:expressions.Exp) : CompilationUnit = {
    val name = e.toString

   // val methods:Seq[MethodDeclaration] = domain.ops.map(methodGenerator(e))
    val visitor:MethodDeclaration = visitorMethodGenerator()
    val atts:Seq[FieldDeclaration] = e.attributes.flatMap(att => Java(s"private ${typeGenerator(att.tpe)} ${att.name};").fieldDeclarations())

    // Builds up the attribute fields and set/get methods. Also prepares for one-line constructor.
    var params:Seq[String] = Seq.empty
    var cons:Seq[String] = Seq.empty
    var methods:Seq[MethodDeclaration] = Seq.empty

    val fields:Seq[FieldDeclaration] = e.attributes.map(att => {
      val capAtt = att.name.capitalize
      val tpe = typeGenerator(att.tpe)

      params = params :+ s"$tpe ${att.name}"
      cons   = cons   :+ s"  this.${att.name} = ${att.name};"

      // make the set/get methods
      methods = methods ++ Java(s"""
                                 |public $tpe get$capAtt() { return ${att.name};}
                                 |public void set$capAtt($tpe val) { this.${att.name} = val; }
                                """.stripMargin).methodDeclarations()

      Java(s"private $tpe ${att.name};").fieldDeclarations().head
    })

    val constructor = Java(s"""|public $name (${params.mkString(",")}) {
                               |   ${cons.mkString("\n")}
                               |}""".stripMargin).constructors().head

    // visitor methods for accepting operations
    // make accept method call and add to class
    Java(s"""|package expression;
             |public class $name extends Exp {
             |
             |  ${constructor.toString}
             |
             |  ${atts.mkString("\n")}
             |  ${methods.mkString("\n")}
             |  ${visitor.toString()}
             |}""".stripMargin).compilationUnit()
  }

//  /** Generate the base class. */
//  def generateBase(domain:Model): CompilationUnit = {
//
//    // Allow for operations to be void; if not an issue in future, then filter out here...
//    val signatures:Seq[MethodDeclaration] = domain.ops.map(op => {
//
//      val ret = if (op.returnType.isDefined) {
//        typeGenerator(op.returnType.get)
//      } else {
//        "void"
//      }
//
//      Java(s"public abstract $ret ${op.name}();").methodDeclarations().head
//    })
//
//    // same every time
//    Java(s"""|package expression;
//             |public abstract class $BASE {
//             |  ${signatures.mkString("\n")}
//             |}""".stripMargin).compilationUnit()
//  }
}







