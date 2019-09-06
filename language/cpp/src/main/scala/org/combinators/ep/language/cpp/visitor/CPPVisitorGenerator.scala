package org.combinators.ep.language.cpp.visitor   /*DI:LD:AD*/

import org.combinators.ep.domain.BaseDomain
import org.combinators.ep.language.cpp._

// visitor based solution
// https://www.codeproject.com/Tips/1018315/%2FTips%2F1018315%2FVisitor-with-the-Return-Value
trait CPPVisitorGenerator extends CPPGenerator with DataTypeSubclassGenerator with CPPBinaryMethod  {

  val domain: BaseDomain with ModelDomain
  import domain._

  def getModel: domain.Model

  /**
    * Generating a visitor OO solution requires:
    * 1. A Class for every exp data type (with Header file)
    * 2. A Base class to be superclass of them all (Exp.h)
    * 3. A visitor base class (IVisitor.h)
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
      generateBaseClass() :+
      defaultHeaderFile() :+
      generateBase(flat) // base class $BASE
  }

  /** For straight design solution, directly access attributes by name. */
  override def expression (exp:DataType, att:Attribute) : Expression = {
    new CPPExpression(s"${att.instance}")
  }

  // TODO: do something about capitalization
  /** Directly access local method, one per operation, with a parameter representing visitor name. */
  override def dispatch(expr:CPPExpression, op:Operation, params:CPPExpression*) : CPPExpression = {
    val args:String = params.mkString(",")
    new CPPExpression(s"(new ${op.concept}(e.get${expr.toString.capitalize}($args)))->getValue()")
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
      new CPPExpression(s"(new $op(&e))->getValue()")
    } else {
      super.contextDispatch(source, delta)
    }
  }

  /**
    * values are stored in local table, indexed by e
    */
  override def result (expr:Expression) : Seq[Statement] = {
    Seq(new CPPStatement(s"value = $expr;"))
  }

  /**
    * For Visitor table, must dereference
    */
  override def valueOf(expr:Expression, params:CPPElement*): CPPExpression = {
    val args:String = params.mkString(",")
    new CPPExpression(s"e.get${expr.toString.capitalize}($args)")
  }

  /** Return designated C++ type associated with type. Use Exp * pointer for structure */
  override def typeConverter(tpe:TypeRep) : CPPType = {
    tpe match {
      case domain.baseTypeRep => new CPPType("Exp *")
      case _ => super.typeConverter(tpe)
    }
  }

  // TODO: Consider removing this function
  /** Computer return type for given operation (or void). */
  def returnType(op:Operation): CPPType = typeConverter(op.returnType)

  /**
    * Operations are implement ala visitor.
    *
    * All results are immediately stored within a value_map_[] structure, indexed by
    * the expression e.
    */
  def methodGenerator(exp:DataType, op:Operation): CPPMethod = {
    val params = parameters(op)
    val stmts = Seq(s"${logic(exp, op).mkString("\n")}")
    new CPPMethod("void", s"Visit", s"(const ${exp.name}& e)", stmts)
  }

  /** Default header file needed for most classes. */
  def defaultHeaderFile() : CPPHeaderCode = {
    new CPPHeaderCode("visitor",
      s"""
         |#ifndef _VISITOR_H_
         |#define _VISITOR_H_
         |#include <iostream>
         |#include <map>
         |#include <memory>
         |#include <sstream>
         |#include <string>
         |#include <vector>
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
    val tpe:CPPType = typeConverter(op.returnType)
    val realType:String = op match {
      case po:ProducerOperation => "Exp *"
      case _ => tpe.name
    }

    val allForwards = getModel.flatten().types.map(exp => s""" #include "${exp.concept}.h" """)
    val extras = dependency(op).map(o => s"""#include "${o.concept}.h" """) ++ allForwards

    // no-arg constructor
    val constructor:Seq[CPPElement] =
      Seq(new CPPConstructor(op.concept, "(const Exp *e)", Seq(new CPPStatement("e->Accept(this);"))))
      //Seq(new CPPElement (s"${op.concept} (Exp *e) { e->Accept(this); }".stripMargin))

    // binary methods?
    val binaryConstructor:Seq[CPPElement] = op match {
      case bm:domain.BinaryMethod =>
        Seq(new CPPConstructor(op.concept, "(const Exp *e, const Exp *t)", Seq(new CPPStatement("that = t; e->Accept(this);"))))
      case _ => Seq.empty
    }

    // visitor stores in field, with accessor method
    val field:Seq[CPPElement] = Seq(new CPPStatement(s"$tpe value;"))
    val accessor:Seq[CPPElement] =
      Seq(new CPPMethod(tpe.toString, "getValue", "()", Seq("return value;")))

    // binary fields?
    val binaryField:Seq[CPPElement] = op match {
      case bm:domain.BinaryMethod => Seq(new CPPStatement (s"""const Exp *that; """))
      case _ => Seq.empty
    }

    new CPPClass (op.concept, op.concept, constructor ++ binaryConstructor ++ signatures ++ accessor,
      binaryField ++ field)
      .setSuperclass("IVisitor")
      .addHeader(extras)
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
      val tpe = typeConverter(att.tpe)

      addedFields = addedFields :+ new CPPStatement(s"$tpe ${att.instance};")

      // prepare for constructor
      params = params :+ s"$tpe ${att.instance}_"
      cons = cons :+ s"${att.instance}(${att.instance}_)"

      // make the set/get methods
      addedMethods = addedMethods :+
        new CPPMethod(tpe.toString, s"get${att.concept}", "()", Seq(s"return ${att.instance};")).setConstant()
    })

    // make constructor
    addedMethods = addedMethods :+
       new CPPConstructor(sub.name, s"(${params.mkString(",")}) : ${cons.mkString(",")}", Seq(new CPPStatement("")))

    // Method declaration (with implementation)
    val visitor =
      new CPPMethod("void", "Accept", "(IVisitor* v)", Seq("v->Visit(*this);")).setConstant()
      //new CPPElement("void Accept(IVisitor* v) { v->Visit(*this); } ")

    // add Binary methods if needed
    val astreeMethod:Seq[CPPElement] = if (getModel.flatten().hasBinaryMethod) {
      Seq(new CPPMethodDeclaration("Tree *", domain.AsTree.instance, "()").setConstant())
      //Seq(new CPPElement (s"""Tree *${domain.AsTree.instance}() const; """))
    } else {
      Seq.empty
    }
    addedMethods = addedMethods :+ visitor
    addedMethods = addedMethods ++ astreeMethod

    new CPPClass(name, name, addedMethods, addedFields)
      .setSuperclass("Exp")
      .addHeader(standardHeader())
      .addHeader(Seq("""#include "Exp.h" """, """#include "IVisitor.h" """))
  }

  /** Generate the full class for the given expression sub-type BUT ONLY for binary methods. */
  def generateExpImpl(model:Model, sub:DataType) : CPPFile = {
    val binaryMethods:Seq[CPPElement] = if (getModel.flatten().hasBinaryMethod) {
      // sub
      val body:String = sub match {
        case _:Unary | _:Binary =>
          val atts = sub.attributes
            .filter(att => att.tpe == domain.baseTypeRep)
            .map(att => s"${att.instance}->astree()").mkString(",")

          s"""
             |    std::vector<Tree *> vec_${sub.concept} = { $atts };
             |    return new Node(vec_${sub.concept}, DefinedSubtypes::${sub.concept}Subtype);
             |""".stripMargin

        case lit:Atomic =>
          s"""
             |    return new Leaf(getValue());    // hard-coded and could be replaced.
             |""".stripMargin

      }
      Seq(new CPPMethod("Tree *", s"${sub.concept}::${domain.AsTree.instance}", "()", Seq(body)).setConstant())
    } else {
      Seq.empty
    }

    val contents =
      s"""|
         |#include "visitor.h"
          |#include "Exp.h"
          |#include "IVisitor.h"
          |#include "${sub.concept}.h"
          |
          |${binaryMethods.mkString("\n")}
       """.stripMargin.split("\n")

    new StandAlone(sub.concept, contents)
  }

  /** Generate the base class, with all operations from flattened history. */
  def generateBase(model:Model): CPPFile = {

    // binary methods?
    val astreeMethod:Seq[CPPElement] = if (getModel.flatten().hasBinaryMethod) {
      Seq(new CPPMethodDeclaration("virtual Tree *", domain.AsTree.instance, "()").setConstant().setVirtual())
    } else {
      Seq.empty
    }

    val astreeHeaders:Seq[String] = if (getModel.flatten().hasBinaryMethod) {
      Seq(""" #include "Tree.h" """)
    } else {
      Seq.empty
    }

    new CPPClass("Exp", "Exp",
      Seq(new CPPMethodDeclaration("virtual void", "Accept", "(IVisitor* v)").setConstant().setVirtual()) ++ astreeMethod, Seq.empty)
      .addHeader(Seq(s"class IVisitor;") ++ astreeHeaders)
  }

  /** For visitor, the base class defines the accept method used by all subclasses. */
  def generateBaseClass():CPPFile = {

    // Ignore passed in model in favor of just grabbing it on demand...
    val allOps = getModel.flatten().types.map(exp =>
      new CPPMethodDeclaration("virtual void", "Visit", s"(const $exp& e)").setVirtual())

    // forward refers
    val allForwards = getModel.flatten().types.map(exp => s"class ${exp.concept};")

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

    //new CPPClass("IVisitor", allOps)
    new CPPClass("IVisitor", "IVisitor", allOps, Seq.empty)
      .addHeader(allForwards ++ moreImports)
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
