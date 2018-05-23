package example.expression.visitor

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.{FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr.SimpleName
import com.github.javaparser.ast.stmt.Statement
import example.expression.j.Operators
import org.combinators.templating.twirl.Java
import expression._
import expression.instances.{UnitSuite, UnitTest}
import shared.compilation.CodeGeneratorRegistry

import scala.collection.JavaConverters._

/** Future work: reduce the need to have full set of traits here, with the internals accessing the code generators. Separate concerns */
trait ExpressionSynthesis extends Operators {

  def testCaseGenerator(op:Operation, identifier:SimpleName, tc: UnitTest) : Seq[Statement]

  /** Construct visitor abstract class. */
  def Visitor(domain:DomainModel) :CompilationUnit = {
      val signatures = domain.data.asScala
        .map(exp => s"public abstract R visit(${exp.getClass.getSimpleName} exp);").mkString("\n")

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

  /** Generate from domain. USER NEEDS TO SPECIFY THESE EITHER AUTOMATICALLY OR MANUALLY */
  def BaseExpClass(): CompilationUnit =
      Java(s"""|package expression;
               |
               |public abstract class Exp {
               |    public abstract <R> R accept(Visitor<R> v);
               |}
               |""".stripMargin).compilationUnit()


  /** Works on any subclass of Exp to produce the base class structure for a sub-type of Exp. */
  def BaseClass(expr:Exp):CompilationUnit = {

    val name = expr.getClass.getSimpleName
    Java(s"""package expression; public class $name extends Exp { }""".stripMargin).compilationUnit()
  }

  /**
    * Construct class to represent subclass of Exp.
    *
    * @param sub    sub-type of Exp (i.e., Lit) for whom implementation class is synthesized.
    */
  def ImplClass(sub:Exp, unit:CompilationUnit): CompilationUnit = {

    // Builds up the attribute fields and set/get methods. Also prepares for one-line constructor.
    var params:Seq[String] = Seq.empty
    var cons:Seq[String] = Seq.empty

    sub.ops.asScala.foreach {
      case att: Attribute =>
        val capAtt = att.attName.capitalize
        val tpe = Type_toString(att.attType)
        val fields:Seq[FieldDeclaration] = Java(s"private $tpe ${att.attName};").fieldDeclarations()
        fields.foreach { x => unit.getTypes.get(0).getMembers.add(x) }

        // prepare for constructor
        params = params :+ s"$tpe ${att.attName}"
        cons   = cons   :+ s"  this.${att.attName} = ${att.attName};"

        // make the set/get methods
        val methods:Seq[MethodDeclaration] = Java(s"""
                                                     |public $tpe get$capAtt() { return ${att.attName};}
                                                     |public void set$capAtt($tpe val) { this.${att.attName} = val; }
                      """.stripMargin).methodDeclarations()

        methods.foreach { x => unit.getTypes.get(0).getMembers.add(x) }

      case _ =>
    }

    // make constructor and add to class
    val constructor = Java(s"""
                              |public ${sub.getClass.getSimpleName} (${params.mkString(",")}) {
                              |   ${cons.mkString("\n")}
                              |}""".stripMargin).constructors().head

    unit.getTypes.get(0).getMembers.add(constructor)

    // make accept method call and add to class
    val visitor = Java (s"""
                           |public <R> R accept(Visitor<R> v) {
                           |   return v.visit(this);
                           |}""".stripMargin).methodDeclarations()

    visitor.foreach { x => unit.getTypes.get(0).getMembers.add(x) }

    unit
  }

  /** Brings in classes for each operation. These can only be completed with the implementations. */
  def OpImpl(subTypes: Seq[Exp], op:Operation, codeGenerator:CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]]]): CompilationUnit = {

    val name = op.getClass.getSimpleName
    val tpe = Type_toString(op.`type`)

    val cg:Option[CodeGeneratorRegistry[Seq[Statement]]] = codeGenerator(op)

    val signatures = if (cg.isEmpty) {
      s"""public $tpe visit(${op.getClass.getSimpleName} e) { // $name operator not defined!"); \n}"""
    } else {

      subTypes.map(exp => {
        val seqStmt: Option[Seq[Statement]] = cg.get(exp)
        if (seqStmt.isDefined) {
          val stmts: String = seqStmt.get.mkString("\n")
          val tpe: String = Type_toString(op.`type`)

          s"public $tpe visit(${exp.getClass.getSimpleName} e) { $stmts }"
        } else {
          s"""public $tpe visit(${exp.getClass.getSimpleName} e) { // ${exp.getClass.getSimpleName}/$name operator not defined!"); \n}"""
        }
      }).mkString("\n")
    }

    val s = Java(s"""|package expression;
                     |public class $name extends Visitor<$tpe>{
                     |$signatures
                     |}""".stripMargin)

    s.compilationUnit()
  }

  /**
    * Construct JUnit test cases for each registered expression.
    *
    * Within the test case method, each of the registered operations are evaluated to confirm the expect value
    * matches the actual value.
    *
    * Sure would like to convert this into a generator, since then (in covariant) I would generate the appropriate
    * instantiation based on the domain model available at the time. Note that this might still be possible,
    * assuming the representationCodeGenerators has access to the top-level domain model.
    */
  def Driver (generator:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression],
              allTests:UnitSuite) :CompilationUnit = {

    // each test creates a public void testXXX method, inside of which, each sub-part is evaluated.
    var testNumber = 0
    val allGen:String = allTests.iterator.asScala.map(tst => {
      // build up expression for which all tests are defined.
      val code:Option[com.github.javaparser.ast.expr.Expression] = generator(tst.expression)
      testNumber = testNumber+1

      val init:String = s"""Exp exp$testNumber = ${code.get.toString};"""
      val ident:SimpleName = Java(s"exp$testNumber").simpleName()

      // each individual test case is evaluated within the context of this expression
      //var resultNumber = 0
      val blocks:String = tst.iterator().asScala.flatMap(tc => testCaseGenerator(tc.op, ident, tc)).mkString("\n")

      val codeBlock:String = s"""|public void testEval$testNumber() {
                                 |  $init
                                 |  $blocks
                                 |}""".stripMargin
      Java(codeBlock).methodDeclarations().mkString("\n")
    }).mkString("\n")

    Java(s"""|package expression;
             |import junit.framework.TestCase;
             |public class TestSuite extends TestCase {
             |    $allGen
             |}""".stripMargin).compilationUnit()
  }
}
