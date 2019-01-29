package example.expression.visitor   /*DI:LD:AD*/

import com.github.javaparser.ast.body.{BodyDeclaration, MethodDeclaration}
import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.j.{DataTypeSubclassGenerator, JavaBinaryMethod, JavaGenerator, OperationAsMethodGenerator}
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait VisitorGenerator extends JavaGenerator with DataTypeSubclassGenerator with OperationAsMethodGenerator with JavaBinaryMethod {
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
              m.ops.map(op => generateOperation(flat, op))) :+    // one class for each op
      generateBaseClass(flat) :+                              // abstract base class
      generateBase(flat)                                      // visitor gets its own class (overriding concept)
  }

  /** Handle self-case here. */
  override def contextDispatch(source:Context, delta:Delta) : Expression = {
    if (delta.expr.isEmpty) {
      dispatch(Java("e").expression(), delta.op.get, delta.params : _ *)
    } else {
      super.contextDispatch(source, delta)
    }
  }

  /**
    * Visitor access attribute by means of (generic) parameter e.getXXX() method.
    *
    * Note: Depends on having an external context which defines the variable e.
    */
  override def expression (exp:domain.Atomic, att:domain.Attribute) : Expression = {
    Java(s"e.get${att.concept}()").expression[Expression]()
  }

  /** Directly access local method, one per operation, with a parameter. */
  override def dispatch(expr:Expression, op:domain.Operation, params:Expression*) : Expression = {
    val args:String = params.mkString(",")
    Java(s"$expr.accept(new ${op.concept}($args))").expression()
  }

  /**
    * Return designated Java type associated with type.
    *
    */
  override def typeConverter(tpe:domain.TypeRep) : Type = {
    tpe match {
      case domain.baseTypeRep => Java(s"${domain.baseTypeRep.name}").tpe()
      case _ => super.typeConverter(tpe)
    }
  }

  /** Return Visitor class, which contains a visit method for each available sub-type. */
  override def generateBase(flat:domain.Model): CompilationUnit = {
    val signatures = flat.types
      .map(exp => s"public abstract R visit(${exp.name} exp);").mkString("\n")

    Java (s"""|package visitor;
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
  def generateBaseClass(flat:domain.Model):CompilationUnit = {
    val binaryTreeInterface = if (flat.ops.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      Java(s"""public abstract tree.Tree ${domain.AsTree.instance}();""").classBodyDeclarations
    } else {
      Seq.empty
    }

    Java(s"""|package visitor;
             |
             |public abstract class ${domain.baseTypeRep.concept} {
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
    val recursiveArgs = exp.attributes.map(att => att.name + s".${domain.AsTree.instance}()").mkString(",")

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
         |public tree.Tree ${domain.AsTree.instance}() {
         |  ${body.mkString("\n")}
         |}""".stripMargin).methodDeclarations()
  }

  /** Generate the full class for the given expression sub-type from flattened model. */
  override def generateExp(flat:domain.Model, exp:domain.Atomic) : CompilationUnit = {
    val name = exp.toString

    val visitor = Java (s"""|public <R> R accept(Visitor<R> v) {
                            |   return v.visit(this);
                            |}""".stripMargin).methodDeclarations()

    // Regardless of model passed in, we need to flatten everything to get any
    // BinaryMethodTreeBase ops. This is only necessary because of extensibleVisitor...
    val helpers:Seq[BodyDeclaration[_]] = if (flat.ops.exists {
      case bm: domain.BinaryMethodTreeBase => true   // was BinaryMethod
      case _ => false
    }) {
      logicAsTree(exp)
    } else {
      Seq.empty
    }

    Java(s"""|package visitor;
             |public class $name extends ${domain.baseTypeRep.name} {
             |
             |  ${constructor(exp)}
             |  ${helpers.mkString("\n")}
             |  ${fields(exp).mkString("\n")}
             |  ${getters(exp).mkString("\n")}
             |  ${visitor.mkString("\n")}
             |}""".stripMargin).compilationUnit
  }

  /**
    * Brings in classes for each operation. These can only be completed with the implementations.
    *
    * Must handle BinaryMethod (Equals) and BinaryMethodBase (Astree) specially.
    */
  def generateOperation(flat:domain.Model, op:domain.Operation): CompilationUnit = {
    val signatures = flat.types.map(exp => methodGenerator(exp, op))

    // if operation has parameters then must add to visitor as well
    val atts = op.parameters.flatMap {
      case (name,atype) => Java(s"${typeConverter(atype)} $name;").fieldDeclarations()
    }

    // We only need to have constructors if we have arguments. On a side note,
    // this is also an important consideration for extensibleVisitor
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

   Java(s"""|package visitor;
            |public class ${op.concept} extends Visitor<$tpe>{
            |  $ctor
            |
            |  ${atts.mkString("\n")}
            |  ${signatures.mkString("\n")}
            |}""".stripMargin).compilationUnit
  }
}
