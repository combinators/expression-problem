package example.expression.cpp


import expression._
import expression.history.History
import expression.types.Types

import scala.collection.JavaConverters._

/** Future work to sanitize combinators to be independent of Exp. */
trait ExpressionSynthesis extends CPPSemanticTypes with HasCPPCodeGenerator {

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

  /** Construct visitor abstract class. */
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

}
