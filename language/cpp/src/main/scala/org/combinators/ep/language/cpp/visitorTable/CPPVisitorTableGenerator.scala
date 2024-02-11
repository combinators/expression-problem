package org.combinators.ep.language.cpp.visitorTable   /*DI:LD:AD*/

import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import org.combinators.ep.language.cpp.{CPPElement, _}

// visitor based solution that uses a hashtable to store partial results of recursive calls.
trait CPPVisitorTableGenerator extends CPPGenerator with DataTypeSubclassGenerator with CPPBinaryMethod {

  val domain: BaseDomain with ModelDomain
  import domain._

  def getModel: domain.Model

  /**
    * Generating a visitor OO solution requires:
    * 1. A Class for every exp data type (with Header file)
    * 2. A Base class to be superclass of them all (Exp.h)
    * 3. A visitor base class (ExpVisitor.h)
    * 4. A visitor subclass for every operation
    */
  def generatedCode(): Seq[CPPFile] = {
    val flat = getModel.flatten()
    val clazzes:Seq[CPPFile] = getModel.inChronologicalOrder   // visitors are constructed in order
      .filter(m => m.ops.nonEmpty)
      .flatMap(m =>
        m.ops.map(op => operationGenerator(flat, op)))         // one class for each op

    flat.types.map(tpe => generateExp(flat, tpe)) ++
      flat.types.map(tpe => generateExpImpl(flat, tpe)) ++
      clazzes :+
      generateBaseClass(flat) :+
      defaultHeaderFile() :+
      generateBase(flat) // base class $BASE
  }

  /**
    * values are stored in local table, indexed by e
    */
  override def result (expr:Expression) : Seq[Statement] = {
    Seq(new CPPStatement(s"value_map_[e] = $expr;"))
  }

  /** For straight design solution, directly access attributes by name. */
  override def expression (exp:DataType, att:Attribute) : Expression = {
    new CPPExpression(s"${att.instance}")
  }

  /** Directly access local method, one per operation, with a parameter. For VisitorTable, must grab from valueMap. */
  // TODO: do something about capitalization
  override def dispatch(expr:CPPExpression, op:Operation, params:CPPExpression*) : CPPExpression = {
    val args:String = params.mkString(",")
    new CPPExpression(s"value_map_[e->get${expr.toString.capitalize}($args)]")
  }

  override def contextDispatch(source:Context, delta:Delta) : Expression = {
    if (source.op.isEmpty) {  // delta.isIndependent
      // a test case. Must then use delta.expr "as is"
      val opargs = if (delta.params.nonEmpty) {
        "," + delta.params.mkString (",")
      } else {
        ""
      }

      new CPPExpression(s"((new ${delta.op.get.concept}(${delta.expr.get}$opargs))->getValue())")
    } else if (delta.expr.isEmpty) {
      val op = delta.op.get.concept
      val args = delta.params.mkString (",")
      new CPPExpression(s"(new $op(e))->getValue()")
    } else if (delta.op.isDefined) {
      val opParams = if (delta.params.nonEmpty) {
        "(" + delta.params.mkString(",") + ")"
      } else {
        ""
      }
      val op = delta.op.get.concept
      new CPPExpression(s"(new $op(value_map_[e->get${delta.expr.get.toString.capitalize}($opParams)]))->getValue()")
    } else {
      super.contextDispatch(source, delta)
    }
  }

  /**
    * For Visitor table, must dereference
    */
  override def valueOf(expr:Expression, params:CPPElement*): CPPExpression = {
    val args:String = params.mkString(",")
    new CPPExpression(s"e->get${expr.toString.capitalize}($args)")
  }

  /** Return designated Java type associated with type, or void if all else fails. */
  override def typeConverter(tpe:TypeRep) : CPPType = {
    tpe match {
      case domain.baseTypeRep => new CPPType("Exp")
      case _ => super.typeConverter(tpe)
    }
  }

  /** Computer return type for given operation (or void). */
  def returnType(op:Operation): CPPType = {
    op.returnType match {
      case Some(tpe) => typeConverter(tpe)
      case _ => new CPPType("void")
    }
  }

  /** Operations are implement ala visitor. */
  def methodGenerator(exp:DataType, op:Operation): CPPMethod = {
    val params = parameters(op)
    new CPPMethod("void", s"Visit", s"(const ${exp.name}* e)", logic(exp, op).mkString("\n"))
  }

  /** Default header file needed for most classes. */
  def defaultHeaderFile() : CPPHeaderCode = {
    new CPPHeaderCode("visitor",
      s"""
         |#ifndef _VISITOR_H_
         |#define _VISITOR_H_
         |#include <iostream>
         |#include <memory>
         |#include <sstream>
         |#include <string>
         |#include <vector>
         |#include <map>
         |#endif /* _VISITOR_H_ */
       """.stripMargin.split("\n"))
  }

  // standard headers
  def standardHeader():Seq[String] = {
    s"""#include "visitor.h" """.stripMargin.split("\n")
  }

  /**
    * Brings in classes for each operation. These can only be completed with the implementations.
    *
    * Must handle BinaryMethod (Equals) and BinaryMethodBase (Astree) specially.
    */
  def operationGenerator(model:domain.Model, op:domain.Operation): CPPFile = {
    val signatures:Seq[CPPMethod] = model.types.map(exp => methodGenerator(exp, op))
    val tpe:CPPType = typeConverter(op.returnType.get)
    val realType:String = op match {
      case po:ProducerOperation => "Exp *"
      case _ => tpe.name
    }
    // access value via lookup into value_map_
    // val _retType:String, val _name:String, val _params:String, val _body:Seq[String]
    val lookup = Seq(
      new CPPMethod(realType, "getValue", "()", Seq("return value_map_[top];")),
      new CPPMethod(realType, "getValue", "(const Exp& e)", Seq("return value_map_[&e];"))
    )

    val extras = dependency(op).map(o => s"""#include "${o.concept}.h" """)

    // no-arg constructor
    val constructor:Seq[CPPElement] =
      Seq(new CPPConstructor (op.concept, "(const Exp *e)", Seq(new CPPStatement("top = e; e->Accept(this);"))))

      //Seq(new CPPMethod (s"${op.concept} (const Exp *e) { top = e; e->Accept(this); }".stripMargin))

    // binary methods?
    val binaryConstructor:Seq[CPPElement] = op match {
      case bm:domain.BinaryMethod =>
        Seq(new CPPConstructor (op.concept, "(Exp *e, Exp *t)", Seq(new CPPStatement("that = t; top = e; e->Accept(this);"))))
//        Seq(new CPPElement (s"""|${op.concept} (Exp *e, Exp *t) {
//                                |    that = t;
//                                |    top = e;
//                                |    e->Accept(this);
//                                |}""".stripMargin))
      case _ => Seq.empty
    }

    // binary fields?
    val binaryField:Seq[CPPElement] = op match {
      case bm:domain.BinaryMethod => Seq(new CPPStatement (s" Exp *that; "))
      case _ => Seq.empty
    }

    val topField:Seq[CPPElement] = Seq(new CPPStatement(s"const Exp *top;"))

    new CPPClass (op.concept, op.concept, constructor ++ lookup ++ binaryConstructor ++ signatures,
      Seq(new CPPStatement(s"""std::map<const Exp*, $realType  > value_map_;""")) ++ topField ++ binaryField)
      .setSuperclass("ExpVisitor")
      .addHeader(Seq("""#include "ExpVisitor.h" """, """#include "visitor.h" """))
      .addHeader(extras)
  }

  /** Generate the full class for the given expression sub-type. */
  def generateExpImpl(model:Model, sub:DataType) : CPPFile = {
    val signatures = sub.attributes
      .filter(att => att.tpe == domain.baseTypeRep)
      .map(att => new CPPStatement(s"${att.instance}->Accept(visitor);")).mkString("\n")

    val binaryMethods:Seq[CPPElement] = if (getModel.flatten().hasBinaryMethod) {
      // sub
      // CPPMethod (val _retType:String, val _name:String, val _params:String, val _body:Seq[String]) extends CPPElement {


      val body:String = sub match {
        case _:Unary | _:Binary => {
          val atts = sub.attributes
            .filter(att => att.tpe == domain.baseTypeRep)
            .map(att => s"${att.instance}->astree()").mkString(",")

          s"""
             |    std::vector<Tree *> vec_${sub.instance} = { $atts };
             |    return new Node(vec_${sub.instance}, DefinedSubtypes::${sub.concept}Subtype);
             |""".stripMargin
        }
        case _:Atomic => {
          s"""
             |    return new Leaf(getValue());    // hard-coded and could be replaced.
             |""".stripMargin
        }
      }
      Seq(new CPPMethod("Tree *", s"${sub.concept}::astree", "()", Seq(body)).setConstant())
//      Seq(new CPPElement(method))
    } else {
      Seq.empty
    }

    val contents =
      s"""|
         |#include "visitor.h"
          |#include "Exp.h"
          |#include "ExpVisitor.h"
          |#include "${sub.concept}.h"
          |void ${sub.concept}::Accept(ExpVisitor* visitor) const {
          |  $signatures
          |  visitor->Visit(this);
          |}
          |${binaryMethods.mkString("\n")}
       """.stripMargin.split("\n")

    new StandAlone(sub.concept, contents)
  }

  /** Generate the full class for the given expression sub-type (except for impl). */
  def generateExp(model:Model, sub:DataType) : CPPFile = {
    val name = sub.name

    // Builds up the attribute fields and set/get methods. Also prepares for one-line constructor.
    var params:Seq[String] = Seq.empty
    var cons:Seq[String] = Seq.empty

    var addedFields:Seq[CPPElement] = Seq.empty
    var addedMethods:Seq[CPPElement] = Seq.empty

    sub.attributes.foreach(att => {
      val capAtt = att.concept
      val tpe = typeConverter(att.tpe)

      val ptr = att.tpe match {
        case domain.baseTypeRep => {
          "*"
        }
        case _ => {
          ""
        }
      }

      addedFields = addedFields :+ new CPPStatement(s"const $tpe$ptr ${att.instance};")

      // prepare for constructor
      params = params :+ s"const $tpe$ptr ${att.instance}_"
      cons = cons :+ s"${att.instance}(${att.instance}_)"

      // make the set/get methods
      addedMethods = addedMethods :+
        new CPPMethod(s"const $tpe$ptr", s"get$capAtt", "()", Seq(s"return ${att.instance};")).setConstant()
//        new CPPElement(s"const $tpe$ptr get$capAtt() const { return ${att.instance}; }")
    })

    // make constructor
    addedMethods = addedMethods :+
      new CPPConstructor(sub.concept, s"(${params.mkString(",")}) : ${cons.mkString(",")}", Seq(new CPPStatement("")))
//      new CPPElement (s"${sub.concept} (${params.mkString(",")}) : ${cons.mkString(",")} {}")

    // Method declaration (not implementation)
    val visitor = new CPPMethodDeclaration ("void", "Accept", "(ExpVisitor* visitor)").setConstant()
//      new CPPElement("void Accept(ExpVisitor* visitor) const;")

    // add Binary methods if needed
    val astreeMethod:Seq[CPPElement] = if (getModel.flatten().hasBinaryMethod) {
      Seq(new CPPMethodDeclaration ("Tree *", domain.AsTree.instance, "()").setConstant())
      //Seq(new CPPElement (s"""Tree *${domain.AsTree.instance}() const; """))
    } else {
      Seq.empty
    }
    addedMethods = addedMethods :+ visitor
    addedMethods = addedMethods ++ astreeMethod

    new CPPClass(name, name, addedMethods, addedFields)
      .setSuperclass("Exp")
      .addHeader(standardHeader())
      .addHeader(Seq("""#include "Exp.h" """, """#include "ExpVisitor.h" """))
  }

  /** Generate the base class, with all operations from flattened history. */
  def generateBase(model:Model): CPPFile = {

    // binary methods?
    val astreeMethod:Seq[CPPElement] = if (getModel.flatten().hasBinaryMethod) {
      Seq(new CPPMethodDeclaration ("virtual Tree *", domain.AsTree.instance, "()").setConstant().setVirtual())
      //Seq(new CPPElement ("""virtual Tree *astree() const = 0;"""))
    } else {
      Seq.empty
    }

    val astreeHeaders:Seq[String] = if (getModel.flatten().hasBinaryMethod) {
      Seq(""" #include "Tree.h" """)
    } else {
      Seq.empty
    }

    new CPPClass("Exp", "Exp",
      Seq(new CPPMethodDeclaration("virtual void", "Accept", "(ExpVisitor* visitor)").setConstant().setVirtual()) ++ astreeMethod, Seq.empty)
      //Seq(new CPPElement(s"""virtual void Accept(ExpVisitor* visitor) const = 0;""")) ++ astreeMethod, Seq.empty)
      .addHeader(Seq(s"""#include "visitor.h" """, s"""class ExpVisitor;""") ++ astreeHeaders)
  }

  /** For visitor, the base class defines the accept method used by all subclasses. */
  def generateBaseClass(model:domain.Model):CPPFile = {

    // Ignore passed in model in favor of just grabbing it on demand...
    val allOps = getModel.flatten().types.map(exp =>
      new CPPMethodDeclaration("virtual void", "Visit", s"(const $exp* e)").setVirtual())
      //new CPPElement(s"""virtual void Visit(const $exp* e) = 0;"""))
    val allHeaders = getModel.flatten().types.map(exp => s"""#include "$exp.h" """)

    val moreImports = if (getModel.flatten().hasBinaryMethod) {
      Seq(
        s"""
           |#include "Tree.h" // Binary Methods needs these include files
           |#include "Node.h"
           |#include "Leaf.h"
           |#include "DefinedSubtypes.h" """.stripMargin)
    } else {
      Seq.empty
    }

    new CPPClass("ExpVisitor", "ExpVisitor", allOps, Seq.empty)
      .addHeader(Seq(s"""#include "visitor.h" """) ++ allHeaders ++ moreImports)
  }

  def generateBinaryMethodHelpers():Seq[CPPFile] = {

    // If BinaryMethodTreeBase, need the declarations here.
    if (getModel.flatten().hasBinaryMethod) {
      declarations
    } else {
      Seq.empty
    }
  }
}
