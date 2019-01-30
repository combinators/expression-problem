package example.expression.cpp.visitor   /*DI:LD:AD*/

import example.expression.cpp._
import example.expression.domain.{BaseDomain, ModelDomain}

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
  override def expression (exp:Atomic, att:Attribute) : Expression = {
    new CPPElement(s"${att.name}")
  }

  /** Directly access local method, one per operation, with a parameter representing visitor name. */
  override def dispatch(expr:CPPElement, op:Operation, params:CPPElement*) : CPPElement = {
    val args:String = params.mkString(",")
    new CPPElement(s"(new ${op.name.capitalize}(e.get${expr.toString.capitalize}($args)))->getValue()")
  }

  /** Standard implementation relies on dependent dispatch. TODO: FIX */
  override def contextDispatch(source:Context, delta:Delta) : Expression = {
    if (source.op.isEmpty) {  // delta.isIndependent
      // a test case. Must then use delta.expr "as is"
      val opargs = if (delta.params.nonEmpty) {
        "," + delta.params.mkString (",")
      } else {
        ""
      }

      new CPPElement(s"((new ${delta.op.get.name.capitalize}(${delta.expr.get}$opargs))->getValue())")
      //new CPPElement(s"${delta.expr.get}->${delta.op.get.name.toLowerCase}()")
    } else if (delta.expr.isEmpty) {
      val op = delta.op.get.name.capitalize
      val args = delta.params.mkString (",")
      new CPPElement(s"(new $op(&e))->getValue()")
    } else {
      super.contextDispatch(source, delta)
    }
  }

  /**
    * values are stored in local table, indexed by e
    */
  override def result (expr:Expression) : Seq[Statement] = {
    Seq(new CPPElement(s"value = $expr;"))
  }

  /**
    * For Visitor table, must dereference
    */
  override def valueOf(expr:Expression, params:CPPElement*): CPPElement = {
    val args:String = params.mkString(",")
    new CPPElement(s"e.get${expr.toString.capitalize}($args)")
  }

  /** Return designated C++ type associated with type. Use Exp * pointer for structure */
  override def typeConverter(tpe:TypeRep) : CPPType = {
    tpe match {
      case domain.baseTypeRep => new CPPType("Exp *")
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

  /**
    * Operations are implement ala visitor.
    *
    * All results are immediately stored within a value_map_[] structure, indexed by
    * the expression e.
    */
  def methodGenerator(exp:Atomic, op:Operation): CPPMethod = {
    val params = parameters(op)
    val stmts = Seq(s"${logic(exp, op).mkString("\n")}")
    new CPPMethod("void", s"Visit", s"(${exp.name}& e)", stmts)
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
    val tpe:CPPType = typeConverter(op.returnType.get)
    val realType:String = op match {
      case po:ProducerOperation => "Exp *"
      case _ => tpe.stmt
    }

    val allForwards = getModel.flatten().types.map(exp => s""" #include "${exp.name.capitalize}.h" """)
    val extras = dependency(op).map(o => s"""#include "${o.name.capitalize}.h" """) ++ allForwards

    // no-arg constructor
    val constructor:Seq[CPPElement] =
      Seq(new CPPElement (s"${op.name.capitalize} (Exp *e) { e->Accept(this); }".stripMargin))

    // binary methods?
    val binaryConstructor:Seq[CPPElement] = op match {
      case bm:domain.BinaryMethod =>
        Seq(new CPPElement (s"""
                               |${op.name.capitalize} (Exp *e, Exp *t) {
                               |    that = t;
                               |    e->Accept(this);
                               |}""".stripMargin))
      case _ => Seq.empty
    }

    // visitor stores in field, with accessor method
    val field:Seq[CPPElement] = Seq(new CPPElement(s"$tpe value;"))
    val accessor:Seq[CPPElement] = Seq(new CPPElement(s"$tpe getValue() { return value; }"))

    // binary fields?
    val binaryField:Seq[CPPElement] = op match {
      case bm:domain.BinaryMethod => Seq(new CPPElement (s""" Exp *that; """))
      case _ => Seq.empty
    }

    new CPPClass (op.name.capitalize, op.name.capitalize, constructor ++ binaryConstructor ++ signatures ++ accessor,
      binaryField ++ field)
      .setSuperclass("IVisitor")
      .addHeader(extras)
  }


  /** Generate the full class for the given expression sub-type (except for impl). */
  def generateExp(model:Model, sub:Atomic) : CPPFile = {
    val name = sub.name

    // Builds up the attribute fields and set/get methods. Also prepares for one-line constructor.
    var params:Seq[String] = Seq.empty
    var cons:Seq[String] = Seq.empty

    var addedFields:Seq[CPPElement] = Seq.empty
    var addedMethods:Seq[CPPElement] = Seq.empty

    sub.attributes.foreach(att => {
      val capAtt = att.name.capitalize
      val tpe = typeConverter(att.tpe)

      addedFields = addedFields :+ new CPPElement(s"$tpe ${att.name};")

      // prepare for constructor
      params = params :+ s"$tpe ${att.name}_"
      cons = cons :+ s"${att.name}(${att.name}_)"

      // make the set/get methods
      addedMethods = addedMethods :+ new CPPElement(s"$tpe get$capAtt() const { return ${att.name}; }")
    })

    // make constructor
    addedMethods = addedMethods :+ new CPPElement (s"${sub.name} (${params.mkString(",")}) : ${cons.mkString(",")} {}")

    // Method declaration (with implementation)
    val visitor = new CPPElement("void Accept(IVisitor* v) { v->Visit(*this); } ")

    // add Binary methods if needed
    val astreeMethod:Seq[CPPElement] = if (getModel.flatten().ops.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      Seq(new CPPElement (s"""Tree *${domain.AsTree.name.toLowerCase}() const; """))
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
  def generateExpImpl(model:Model, sub:Atomic) : CPPFile = {
    val binaryMethods:Seq[CPPElement] = if (getModel.flatten().ops.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      // sub
      val method:String = sub match {
        case _:Unary | _:Binary =>
          val atts = sub.attributes
            .filter(att => att.tpe == domain.baseTypeRep)
            .map(att => s"${att.name}->astree()").mkString(",")

          s"""
             |Tree *${sub.name.capitalize}::astree() const {
             |    std::vector<Tree *> vec_${sub.name} = { $atts };
             |    return new Node(vec_${sub.name.capitalize}, DefinedSubtypes::${sub.name.capitalize}Subtype);
             |}""".stripMargin

        case lit:Atomic =>
          s"""
             |Tree *${sub.name.capitalize}::astree() const {
             |    return new Leaf(getValue());    // hard-coded and could be replaced.
             |}""".stripMargin

      }

      Seq(new CPPElement(method))
    } else {
      Seq.empty
    }

    val contents =
      s"""|
         |#include "visitor.h"
          |#include "Exp.h"
          |#include "IVisitor.h"
          |#include "${sub.name.capitalize}.h"
          |
          |${binaryMethods.mkString("\n")}
       """.stripMargin.split("\n")

    new StandAlone(sub.name.capitalize, contents)
  }

  /** Generate the base class, with all operations from flattened history. */
  def generateBase(model:Model): CPPFile = {

    // binary methods?
    val astreeMethod:Seq[CPPElement] = if (getModel.flatten().ops.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      Seq(new CPPElement ("""virtual Tree *astree() const = 0;"""))
    } else {
      Seq.empty
    }

    val astreeHeaders:Seq[String] = if (getModel.flatten().ops.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      Seq(""" #include "Tree.h" """)
    } else {
      Seq.empty
    }

    new CPPClass("Exp", "Exp",
      Seq(new CPPElement(s"""virtual void Accept(IVisitor* v) = 0;""")) ++ astreeMethod, Seq.empty)
      .addHeader(Seq(s"class IVisitor;") ++ astreeHeaders)
  }

  /** For visitor, the base class defines the accept method used by all subclasses. */
  def generateBaseClass():CPPFile = {

    // Ignore passed in model in favor of just grabbing it on demand...
    val allOps = getModel.flatten().types.map(exp =>
        new CPPElement(s"virtual void Visit($exp& e) = 0;"))

    // forward refers
    val allForwards = getModel.flatten().types.map(exp => s"class ${exp.name.capitalize};")

    val moreImports = if (getModel.flatten().ops.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
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
    if (getModel.flatten().ops.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      declarations
    } else {
      Seq.empty
    }
  }

}
