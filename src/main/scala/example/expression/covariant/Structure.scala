package example.expression.covariant

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.{FieldDeclaration, MethodDeclaration}
import org.combinators.templating.twirl.Java
import example.expression.j.Operators
import expression._
import expression.types.Types

import scala.collection.JavaConverters._

/** Use Modularity2016 Java solution. Built from same domain model. */
trait Structure extends Operators {

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
  def SubInterface(sub: Exp): CompilationUnit = {
    val name = sub.getClass.getSimpleName

    val unit = Java(
      s"""
         |package covariant;
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
  def FinalClass(sub: Exp): CompilationUnit = {
      val name = sub.getClass.getSimpleName

      val unit = Java(s"""
                     |package covariant;
                     |public class ${name}Final implements $name {}
                      """.stripMargin).compilationUnit()

      var params: Seq[String] = Seq.empty
      var cons: Seq[String] = Seq.empty

      sub.ops.asScala.foreach {
        case att: Attribute =>
          val tpe = Type_toString(att.attType)

          val fields = Java(s"""
               |private $tpe ${att.attName};
               |""".stripMargin).fieldDeclarations()
          fields.foreach { x => unit.getTypes.get(0).getMembers.add(x) }

          // prepare for constructor
          params = params :+ s"$tpe ${att.attName}"
          cons = cons :+ s"  this.${att.attName} = ${att.attName};"

          val methods = Java(s"""
                             |public $tpe ${att.attName}() { return ${att.attName};}
                            """.stripMargin).methodDeclarations()

          methods.foreach { x => unit.getTypes.get(0).getMembers.add(x) }

        case _: FunctionMethod =>
      }

      // Builds up the attribute fields and set/get methods. Also prepares for one-line constructor.

      // make constructor
      val constructor = Java(s"""
                 |public ${sub.getClass.getSimpleName}Final (${params.mkString(",")}) {
                 |   ${cons.mkString("\n")}
                 |}""".stripMargin).constructors().head

      unit.getTypes.get(0).getMembers.add(constructor)

      unit
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
  def FinalMultiClass(ops: List[Operation], sub: Exp): CompilationUnit = {
      val name = sub.getClass.getSimpleName
      val combined = ops.map(_.getClass.getSimpleName).mkString("")

      val unit = Java(s"""|package covariant;
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

  //  interface ExpPC extends ExpP, ExpC{}
  //  interface LitPC extends ExpPC, LitP, LitC{}
  //  interface AddPC extends ExpPC, AddP, AddC {
  //    ExpPC e1(); ExpPC e2();
  //  }
  def AddMultiOperation(ops: List[Operation], exp: Exp): CompilationUnit = {
      val name = exp.getClass.getSimpleName

      val combined: String = ops.map(_.getClass.getSimpleName).mkString("")
      val commas: String = ops.map(name + _.getClass.getSimpleName).mkString(",")

      val unit: CompilationUnit = Java(
        s"""|package covariant;
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

  // interface ExpP extends Exp { String print(); }
  def AddOperation(op: Operation): CompilationUnit = {
      val name = op.getClass.getSimpleName

      val unit: CompilationUnit = Java(
        s"""
           |package covariant;
           |interface $name extends Exp { }
           |""".stripMargin).compilationUnit()

      val tpe = Type_toString(op.`type`)

      val methods: Seq[MethodDeclaration] = Java(s"""$tpe ${op.name}();""").methodDeclarations()

      methods.foreach { x => unit.getTypes.get(0).getMembers.add(x) }

      unit
    }

  // interface ExpP extends Exp { String print(); }
  def AddMultiOperationInterface(ops: List[Operation]): CompilationUnit = {
      val combined: String = ops.map(_.getClass.getSimpleName).mkString("")
      val names: String = ops.map(_.getClass.getSimpleName).mkString(",")


      val unit: CompilationUnit = Java(
        s"""
           |package covariant;
           |interface $combined extends $names { }
           |""".stripMargin).compilationUnit()

      unit
    }

  /** Generate from domain. */
  def BaseExpInterface: CompilationUnit = {

    val unit: CompilationUnit = Java(s"""
             |package covariant;
             |interface Exp { }
             |""".stripMargin).compilationUnit()

    val func = new FunctionMethod("eval", Types.Double)
    val tpe = Type_toString(func.returnType)

    Java(s"""$tpe ${func.name}();""").methodDeclarations()
      .foreach { x => unit.getTypes.get(0).getMembers.add(x) }

    unit
  }

}