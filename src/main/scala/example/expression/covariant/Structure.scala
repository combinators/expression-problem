package example.expression.covariant

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.{FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.stmt.Statement
import org.combinators.cls.interpreter.{ReflectedRepository, combinator}
import org.combinators.cls.types.Type
import org.combinators.cls.types.syntax._
import org.combinators.templating.twirl.Java
import example.expression.j.MethodMapper
import example.expression.{Base, ExpressionDomain}
import expression.extensions._
import expression._
import expression.history.History
import expression.operations.SimplifyExpr
import expression.types.Types
import shared.compilation.CodeGeneratorRegistry

import scala.collection.JavaConverters._

/** Use Modularity2016 Java solution. Built from same domain model. */
trait Structure extends Base with SemanticTypes with MethodMapper {

  /** Add dynamic combinators as needed. */
  override def init[G <: ExpressionDomain](gamma: ReflectedRepository[G], history: History): ReflectedRepository[G] = {
    var updated = super.init(gamma, history)

    // Every extension needs its own FinalClass and interface
    history.asScala.foreach (domain =>
      domain.data.asScala.foreach {
        sub: Exp => {
          updated = updated
            .addCombinator(new FinalClass(sub))
            .addCombinator(new SubInterface(sub))
        }
      }
    )

    // all non-empty subsets of operations need their own class and operations
    //val subsets:List[List[Operation]] = model.ops.asScala.toSet[Operation].subsets.map(_.toList).toList.filter(_.nonEmpty)
    val subsets:List[List[Operation]] = history.flatten.ops.asScala.toSet[Operation].subsets.map(_.toList).toList.filter(_.nonEmpty)

    subsets.foreach {
      sub: List[Operation] => {

        // sort to be able to ensure className concatenation is proper
        val sorted = sub.sortWith(_.getClass.getSimpleName < _.getClass.getSimpleName)
        if (sorted.length == 1) {
          updated = updated.addCombinator(new AddOperation(sorted.head))
        } else {
          // every subset gets its own interface
          updated = updated.addCombinator(new AddMultiOperationInterface(sorted))

          history.asScala.foreach (domain =>
            domain.data.asScala.foreach { exp: Exp => {
              val st: Type = ep(ep.interface, exp, sorted)
              // ep(ep.interface, exp, ops)
              updated = updated
                .addCombinator(new AddMultiOperation(sorted, exp))
            }
            })
        }

        // do this in all cases...
        history.asScala.foreach (domain =>
          domain.data.asScala.foreach { exp: Exp => {
            updated = updated
              .addCombinator(new FinalMultiClass(sorted, exp))
          }
          }
        )
      }
    }

    // implementations of operations: have to be defined. Note that these "raw Scala methods" could be replaced with tabular tool
    //
    //
    //  Eval     x  Lit, Neg, Add, Sub  ...  Mult Divide ...
    //  Print    |  pl,  pn,  pa,  ps
    //  Collect  |  cl,  cn,  ca,  cs   ...
    //  Simplify |  sl,  sn,  sa,  ss   ...


    // Should be able to cut implementation in half; note the duplicate combinators with Lists of
    // parameters, and sometimes with a single parameter.


//    // Row entries for a given operation as expressed by the different column types
//    def registerImpl(op: Operation, fm: FunctionMethod): Unit = {
//      history.asScala.foreach(domain =>
//      domain.data.asScala
//        .foreach(exp => {
//          val comb: Seq[Statement] = new CodeGenerators().evalGenerators(exp).get
//
//          updated = updated
//            .addCombinator(new AddDefaultImpl(op, fm, exp, comb))
//        })
//      )
//    }

//    def registerExtension(op: Operation, codegen: CodeGeneratorRegistry[Seq[Statement]]): Unit = {
//      history.asScala.foreach (domain =>
//        domain.data.asScala
//          .foreach(exp => {
//            val comb: Seq[Statement] = codegen(exp).get
//
//            updated = updated
//              .addCombinator(new AddExpOperation(exp, op, comb))
//          })
//        )
//    }

    // HACK: TODO: Fix and expose to be configurable
    // note default 'Eval' operation is handled specially since it is assumed to always exist in top Exp class
   // registerImpl(new Eval, new FunctionMethod("eval", Types.Double))

//    // extension
//    registerExtension(new PrettyP, new CodeGenerators().prettypGenerators)
//
//    registerExtension(new Collect, new CodeGenerators().collectLitGenerators)
//
//    // Get class that contains just PrettyP and SimplifyExp
//    val subTypes:String = List(new PrettyP().getClass.getSimpleName,
//                               new SimplifyExpr().getClass.getSimpleName)
//        .sortWith(_ < _)
//        .mkString("")
//    registerExtension(new SimplifyExpr, new SimplifyCodeGenerators(subTypes).simplifyGenerators)

    updated
  }


  /**
    * Construct class to represent subclass of Exp.
    *
    * interface Lit extends Exp {
    * int x();
    * default int eval() { return x(); }
    * }
    *
    * but also recursive types:
    *
    * interface Add extends Exp {
    * Exp e1(); Exp e2();
    * default int eval() {
    * return e1().eval() + e2().eval();   $IMPLEMENT[
    * }
    * }
    *
    * addImpl(new Eval, new Add, Java(s"""return e1().eval() + e2().eval();""").statements())
    * addImpl(new Eval, new Lit, Java(s"""return x();""").statements())
    *
    * @param sub Exp subclass whose interface we are generating
    */
  class SubInterface(sub: Exp) {
    def apply(): CompilationUnit = {
      val name = sub.getClass.getSimpleName

      val unit = Java(
        s"""
           |package ep;
           |public interface $name extends Exp {}
            """.stripMargin).compilationUnit()

      sub.ops.asScala.foreach {
        case att: Attribute =>
          val tpe = Type_toString(att.attType)

          val fields: Seq[MethodDeclaration] = Java(s"""$tpe ${att.attName}();""").methodDeclarations()

          fields.foreach { x => unit.getTypes.get(0).getMembers.add(x) }

        case _: FunctionMethod =>
      }

      unit
    }

    val semanticType: Type = ep(ep.interface, sub)
  }



  /**
    * i.e., LitFinal
    *
    * public class LitFinal implements Lit {
    * Integer value;
    * public LitFinal(int value) { this.value = value; }
    * public Integer value() { return value; }
    * }
    *
    * @param sub type (i.e., "Lit") for which *Final class is to be synthesized.
    */
  class FinalClass(sub: Exp) {
    def apply(): CompilationUnit = {
      val name = sub.getClass.getSimpleName

      val unit = Java(
        s"""
           |package ep;
           |public class ${name}Final implements $name {}
       """.stripMargin).compilationUnit()

      var params: Seq[String] = Seq.empty
      var cons: Seq[String] = Seq.empty

      sub.ops.asScala.foreach {
        case att: Attribute =>
          val tpe = Type_toString(att.attType)

          val fields = Java(
            s"""
               |private $tpe ${att.attName};
               |""".stripMargin).fieldDeclarations()
          fields.foreach { x => unit.getTypes.get(0).getMembers.add(x) }

          // prepare for constructor
          params = params :+ s"$tpe ${att.attName}"
          cons = cons :+ s"  this.${att.attName} = ${att.attName};"

          val methods = Java(
            s"""
               |public $tpe ${att.attName}() { return ${att.attName};}
              """.stripMargin).methodDeclarations()

          methods.foreach { x => unit.getTypes.get(0).getMembers.add(x) }

        case _: FunctionMethod =>
      }

      // Builds up the attribute fields and set/get methods. Also prepares for one-line constructor.

      // make constructor
      val constructor = Java(
        s"""
           |public ${sub.getClass.getSimpleName}Final (${params.mkString(",")}) {
           |   ${cons.mkString("\n")}
           |}""".stripMargin).constructors().head

      unit.getTypes.get(0).getMembers.add(constructor)

      unit
    }

    val semanticType: Type = ep(ep.finalType, sub)
  }

  /**
    * i.e., LitFinal
    *
    *
    **
    *class AddCFinal implements AddC {
    * ExpC e1, e2;
    * AddCFinal(ExpC e1, ExpC e2) {
    *this.e1 = e1;
    *this.e2 = e2;
    * }
    * public ExpC e1() { return e1; }
    * public ExpC e2() { return e2; }
    * }
    *
    * FinalMultiClass(new Add, List(new Collect))
    *
    * @param sub Type (i.e., Add) for which a final class is to be constructed...
    * @param ops ...based on a List[Operation] that specifies desired capabilities
    */
  class FinalMultiClass(ops: List[Operation], sub: Exp) {
    def apply(): CompilationUnit = {
      val name = sub.getClass.getSimpleName
      val combined = ops.map(_.getClass.getSimpleName).mkString("")

      val unit = Java(s"""|package ep;
                          |public class $name${combined}Final implements $name$combined {}
                          """.stripMargin).compilationUnit()

      var params: Seq[String] = Seq.empty
      var cons: Seq[String] = Seq.empty

      sub.ops.asScala.foreach {
        case att: Attribute =>

          // override as needed to deal with co-variant specializations
          var revisedTypeName = Type_toString(att.attType)
          if (att.attType == Types.Exp) {
            revisedTypeName = combined
          }
          val fields: Seq[FieldDeclaration] = Java(s"""
                                                 |private $revisedTypeName ${att.attName};
                                                 |""".stripMargin).fieldDeclarations()
          fields.foreach { x => unit.getTypes.get(0).getMembers.add(x) }

          // prepare for constructor
          params = params :+ s"$revisedTypeName ${att.attName}"
          cons = cons :+ s"  this.${att.attName} = ${att.attName};"

          val methods: Seq[MethodDeclaration] = Java(
            s"""
               |public $revisedTypeName ${att.attName}() { return ${att.attName};}
              """.stripMargin).methodDeclarations()

          methods.foreach { x => unit.getTypes.get(0).getMembers.add(x) }


        case _: FunctionMethod =>
      }

      // Builds up the attribute fields and set/get methods. Also prepares for one-line constructor.

      // make constructor
      val constructor = Java(
        s"""
           |public $name${combined}Final (${params.mkString(",")}) {
           |   ${cons.mkString("\n")}
           |}""".stripMargin).constructors().head

      unit.getTypes.get(0).getMembers.add(constructor)

      unit
    }

    val semanticType: Type = ep(ep.finalType, sub, ops)
  }


  //  interface ExpPC extends ExpP, ExpC{}
  //  interface LitPC extends ExpPC, LitP, LitC{}
  //  interface AddPC extends ExpPC, AddP, AddC {
  //    ExpPC e1(); ExpPC e2();
  //  }
  class AddMultiOperation(ops: List[Operation], exp: Exp) {
    def apply(): CompilationUnit = {
      val name = exp.getClass.getSimpleName

      val combined: String = ops.map(_.getClass.getSimpleName).mkString("")
      val commas: String = ops.map(name + _.getClass.getSimpleName).mkString(",")

      val unit: CompilationUnit = Java(
        s"""|package ep;
            |interface $name$combined extends $combined,$commas { }
            |""".stripMargin).compilationUnit()

      // grab any Exp operations and be sure they appear as $combined
      exp.ops.asScala.foreach {
        case att: Attribute =>
          // only redefine if originally the Exp field.
          if (att.attType == Types.Exp) {
            val fields: Seq[MethodDeclaration] = Java(s"""$combined ${att.attName}();""").methodDeclarations()

            fields.foreach { x => unit.getTypes.get(0).getMembers.add(x) }
          }

        case _: FunctionMethod =>
      }

      unit
    }

    val semanticType: Type = ep(ep.interface, exp, ops)
  }

  // interface ExpP extends Exp { String print(); }
  class AddOperation(op: Operation) {
    def apply(): CompilationUnit = {
      val name = op.getClass.getSimpleName

      val unit: CompilationUnit = Java(
        s"""
           |package ep;
           |interface $name extends Exp { }
           |""".stripMargin).compilationUnit()

      val tpe = Type_toString(op.`type`)

      val methods: Seq[MethodDeclaration] = Java(s"""$tpe ${op.name}();""").methodDeclarations()

      methods.foreach { x => unit.getTypes.get(0).getMembers.add(x) }

      unit
    }

    val semanticType: Type = ep(ep.interface, op)
  }


  // interface ExpP extends Exp { String print(); }
  class AddMultiOperationInterface(ops: List[Operation]) {
    def apply(): CompilationUnit = {
      val combined: String = ops.map(_.getClass.getSimpleName).mkString("")
      val names: String = ops.map(_.getClass.getSimpleName).mkString(",")


      val unit: CompilationUnit = Java(
        s"""
           |package ep;
           |interface $combined extends $names { }
           |""".stripMargin).compilationUnit()

      unit
    }

    val semanticType: Type = ep(ep.interface, ops)
  }


  /** Generate from domain. */
  @combinator object BaseExpInterface {

    // no longer does the default Exp have an add Operation
    val exp: Exp = new Exp

    def apply(): CompilationUnit = {
      val unit: CompilationUnit = Java(
        s"""
           |package ep;
           |interface Exp { }
           |""".stripMargin).compilationUnit()

      // overkill but should work
      // create a sequence with just the Eval operator
      val evalOnly: Seq[FunctionMethod] = Seq.empty :+ new FunctionMethod("eval", Types.Double)

      evalOnly.foreach {
        case func: FunctionMethod =>
          val tpe = Type_toString(func.returnType)

          val methods: Seq[MethodDeclaration] = Java(s"""$tpe ${func.name}();""").methodDeclarations()

          methods.foreach { x => unit.getTypes.get(0).getMembers.add(x) }

        case _ =>
      }

      unit
    }

    val semanticType: Type = ep(ep.interface, exp)
  }

}