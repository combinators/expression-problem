package example.expression.visitor

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.{FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.stmt.Statement
import example.expression.j.MethodMapper
import example.expression.{Base, ExpressionDomain}
import expression.data.{Eval, Lit}
import expression.history.History
import expression.{Attribute, Exp, Operation}
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.cls.types.Type
import org.combinators.templating.twirl.Java
import org.combinators.cls.types.syntax._
import shared.compilation.{CodeGeneratorRegistry, HasCodeGenerator}

import scala.collection.JavaConverters._

trait InitializeRepository
  extends Base
  with HasCodeGenerator
  with SemanticTypes
  with MethodMapper {

  // will be provided
  def codeGenerator: CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]]] =
    CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]]]

  // dynamic combinators added as needed
  override def init[G <: ExpressionDomain](gamma : ReflectedRepository[G], hist:History) :  ReflectedRepository[G] = {
    var updated = gamma

    // update combinators as needed.
    hist.asScala.foreach (domain =>
      updated = domain.data.asScala.foldLeft(updated) {
        case (repo, sub) => repo.addCombinator(new BaseClass(sub)).addCombinator(new ImplClass(sub))
      }
    )

    // need all subtypes from history for the visitor interface
    val allSubTypes:Seq[Exp] = hist.flatten.data.asScala.foldLeft(Seq.empty[Exp]) {
      case (combined, sub) => combined :+ sub
    }


    hist.asScala.foreach (domain =>
      updated = domain.ops.asScala.foldLeft(updated) {
        case (repo, op) => repo.addCombinator(new OpImpl(allSubTypes, op))
      }
    )

    updated
  }

  /** Works on any subclass of Exp to produce the base class structure for a sub-type of Exp. */
  class BaseClass(expr:Exp) {
    def apply(): CompilationUnit = {

      val name = expr.getClass.getSimpleName
      Java(s"""package expression; public class $name extends Exp { }""".stripMargin).compilationUnit()
    }

    // semantic type is based on the subclass (i.e., it will be exp('Base, 'Lit) or exp('Base, 'Add)
    val semanticType:Type = exp(exp.base, expr)
  }

  /**
    * Construct class to represent subclass of Exp.
    *
    * @param sub    sub-type of Exp (i.e., Lit) for whom implementation class is synthesized.
    */
  class ImplClass(sub:Exp) {
    def apply(unit:CompilationUnit): CompilationUnit = {

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

    val semanticType:Type = exp(exp.base, sub) =>: exp(exp.visitor,sub)
  }

  /** Brings in classes for each operation. These can only be completed with the implementations. */
  class OpImpl(subTypes: Seq[Exp], op:Operation) {
    def apply: CompilationUnit = {

      val name = op.getClass.getSimpleName
      val tpe = Type_toString(op.`type`)

//      val methods:Map[Class[_ <: Exp],MethodDeclaration] = Registry.getImplementation(op)
//      val mds:Iterable[MethodDeclaration] = methods.values
//      val signatures = mds.mkString("\n")

      val signatures = subTypes.map (exp => {
          val seqStmt:Seq[Statement] = codeGenerator(op).get(exp).get    // DETECT ERROR by trying to extract
          val stmts:String = seqStmt.mkString("\n")
          val tpe:String = Type_toString(op.`type`)

          s"public $tpe visit(${exp.getClass.getSimpleName} e) { $stmts }"
         }
      ).mkString("\n")

      val s = Java(s"""|package expression;
                       |public class $name extends Visitor<$tpe>{
                       |$signatures
                       |}""".stripMargin)

      s.compilationUnit()
    }

    val semanticType:Type = ops (ops.visitor,op)
  }
}
