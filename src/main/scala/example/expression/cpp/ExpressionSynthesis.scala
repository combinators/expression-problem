package example.expression.cpp


import expression._
import expression.history.History
import expression.instances.UnitSuite
import expression.types.Types
import shared.compilation.CodeGeneratorRegistry

import scala.collection.JavaConverters._

/** Future work to sanitize combinators to be independent of Exp. */
trait ExpressionSynthesis extends InstanceContext with CPPSemanticTypes with HasCPPCodeGenerator with HasCPPTestCaseGenerator {

  def history:History

  def StandardHeaderFile:CPPHeaderCode = {
    var body:Seq[String] = s"""
                              |#ifndef _VISITOR_H_
                              |#define _VISITOR_H_
                              |#include <iostream>
                              |#include <map>
                              |#include <memory>
                              |#include <sstream>
                              |#include <string>
                              |#endif /* _VISITOR_H_ */
                              |""".stripMargin.split("\n")


    //    history.iterator.asScala.foreach(domain =>
    //      domain.data.asScala.foreach(exp => {
    //        body = body :+ s"\nclass ${exp.getClass.getSimpleName};"
    //      })
    //    )

    new CPPHeaderCode ("visitor", Seq.empty)
      .addHeader(body)
  }

  /**
    * Construct visitor abstract class.
    *
    * In this visitor example, there is no return value. Instead, there is an internal map constructed
    * in the visitor object, and one calls getValue(&e) on the top level expression to retrieve the
    * actual value.
    */
  def Visitor: CPPClass = {
    var signatures:Seq[CPPElement] = Seq.empty
    var extraImports:Seq[String] = Seq.empty

    history.iterator.asScala.foreach(domain =>
      domain.data.asScala.foreach(exp => {
        val name = exp.getClass.getSimpleName
        signatures = signatures :+ new CPPElement(s"virtual void Visit$name(const $name* e) = 0;")
        extraImports = extraImports :+ s"""#include "$name.h" """
      }
      ))

    /*
     * A concrete visitor describes a concrete operation on expressions. There is one visit
     * method per type in the class hierarchy.
     */
    new CPPClass("ExpVisitor", "ExpVisitor", signatures, Seq.empty)
      .addHeader(standardHeader())
      .addHeader(extraImports)
  }

  //
  //    val semanticType:Type = generated(generated.visitor)
  //  }

  /** Generic Expr base class with visitor. Produce as Header file Exp.h, not Exp.cpp */
  def BaseExpClass: CPPClass = {

    new CPPClass("Exp", "Exp", Seq(new CPPElement("virtual void Accept(ExpVisitor* visitor) const = 0;")), Seq.empty)
      .addHeader(standardHeader())
      .addHeader(Seq("""class ExpVisitor;""")  // C++ dependency
      )
  }

  //    val semanticType:Type = exp(exp.base, new Exp)
  //  }

  /** Defines classes for each operation */
  def OpDecl(op:Operation) : CPPClass = {

    val name = op.getClass.getSimpleName
    val tpe = Type_toString(op.`type`)

    //implementations
    //val methods:Map[Class[_ <: Exp],String] = getImplementation(op)
    var methods: Seq[CPPMethod] = Seq.empty
    history.iterator.asScala.foreach(domain =>
      domain.data.asScala.foreach(exp => {
        methods = methods :+ codeGenerator(op).get(exp).get
      })
    )

    // each visitor stores local values for access. Hah! have to leave a space after $tpe
    // just in case it would end in a ">"
    // if Exp, then add * otherwise leave alone
    val star = op.`type` match {
      case Types.Exp => "*"

      case _ => ""
    }

    val base: Seq[CPPElement] = Seq(new CPPElement(s"std::map<const Exp*, $tpe $star > value_map_;"))

    // need method for accessing these local values
    val body: Seq[CPPMethod] = Seq(new CPPMethod(tpe, s"${star}getValue", "(const Exp& e)", "return value_map_[&e];")) ++ methods

    new CPPClass(name, name, body, base)
      .setSuperclass("ExpVisitor")
      .addHeader(Seq("#include \"ExpVisitor.h\""))
      .addHeader(standardHeader())
  }

  /** Defines classes for each operation */
  def OpImpl(op:Operation) : CPPCode = {

    val name = op.getClass.getSimpleName
    val tpe = Type_toString(op.`type`)

    //implementations
    //val methods:Map[Class[_ <: Exp],String] = getImplementation(op)
    var methods:Seq[CPPMethod] = Seq.empty
    history.iterator.asScala.foreach(domain =>
      domain.data.asScala.foreach(exp => {
        methods = methods :+ codeGenerator(op).get(exp).get
      })
    )

    // each visitor stores local values for access. Hah! have to leave a space after $tpe
    // just in case it would end in a ">"
    // if Exp, then add * otherwise leave alone
    val star = op.`type` match {
      case Types.Exp => "*"

      case _ => ""
    }

    val base:Seq[CPPElement] = Seq(new CPPElement(s"std::map<const Exp*, $tpe $star > value_map_;"))

    // need method for accessing these local values
    val body:Seq[CPPMethod] = Seq(new CPPMethod (tpe, s"${star}getValue", "(const Exp& e)", "return value_map_[&e];")) ++ methods

    new CPPCode(name, body)  // base?
      .addHeader(standardHeader())
      .addHeader(Seq(s"""#include "$name.h" """))

  }
  //
  //    val semanticType:Type = ops (ops.visitor,op)
  //  }

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
  def TestSuite (generator:CodeGeneratorRegistry[Context],
                 allTests:UnitSuite) : CPPCode = {

    // each test creates a public void testXXX method, inside of which, each sub-part is evaluated.
    var testNumber = 0
    val allGen:String = allTests.iterator.asScala.map(tst => {
      // build up expression for which all tests are defined.
      val code:Option[Context] = generator(tst.expression)
      testNumber = testNumber+1

      val init:String = code.get.toStatement
      //val ident:String = s"exp$testNumber"
      val ident = code.get.name + code.get.id

      // each individual test case is evaluated within the context of this expression
      //var resultNumber = 0
      val blocks:String = tst.iterator().asScala.flatMap(tc => testCaseGenerator(tc.op, ident, tc)).mkString("\n")

      new CPPElement(
        s"""
           | TEST(FirstTestGroup, $ident)
           |      {
           |        $init
           |        $blocks
           |      }
         """.stripMargin)

    }).mkString("\n")

    var methods:Seq[CPPElement] = Seq.empty

    methods = methods :+ new CPPElement(s"""|TEST_GROUP(FirstTestGroup)
                                            |{
                                            |};
                                         """.stripMargin)

    methods = methods :+ new CPPElement(allGen)

    methods = methods :+ new CPPElement(
      s"""
         |int main(int ac, char** av)
         |{
         |  return CommandLineTestRunner::RunAllTests(ac, av);
         |}
         |
       """.stripMargin)

    val ops_includes = history.flatten.ops.asScala.map(op => s"""#include "${op.getClass.getSimpleName}.h" """).mkString("\n")

    //  TODO: Fix the hack below which requires ops to be inserted.
    val cc:CPPCode = new CPPCode("TestCase", methods)
      .addHeader(s"""
                   |#include "CppUTest/TestHarness.h"
                   |#include "CppUTest/SimpleString.h"
                   |#include "CppUTest/PlatformSpecificFunctions.h"
                   |#include "CppUTest/TestMemoryAllocator.h"
                   |#include "CppUTest/MemoryLeakDetector.h"
                   |#include "CppUTest/CommandLineTestRunner.h"
                   |
                   |#include "Exp.h"
                   |#include "ExpVisitor.h"
                   |$ops_includes
                 """.stripMargin.split("\n"))

    cc
  }

}
