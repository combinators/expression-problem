package example.expression.trivially

import com.github.javaparser.ast.{CompilationUnit, Modifier}
import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.body.{FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr.{Expression, NameExpr, SimpleName}
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.Domain
import example.expression.j.{AbstractGenerator, DataTypeSubclassGenerator}
import org.combinators.templating.twirl.Java

trait TriviallyGenerator extends example.expression.Straight.StraightGenerator {
  import domain._

  override def subExpressions(exp: domain.expressions.Exp): Map[String, Expression] = {
    exp.attributes.map(att => att.name -> Java(s"get${att.name.capitalize}()").expression[Expression]()).toMap
  }

  def attrTypeGenerator(currentClass: SimpleName, tpe: types.Types): Type = {
    tpe match {
      case types.Exp => Java(s"$currentClass").tpe()
      case _ => typeGenerator(tpe)
    }
  }

  def baseInterfaceName(op: Operation): SimpleName = {
    Java(s"Exp${op.name.capitalize}").simpleName()
  }

  override def generateExp(domain:Model, exp:expressions.Exp) : CompilationUnit = {
    val name = Java(s"${exp.name}").simpleName()

    val atts:Seq[FieldDeclaration] = exp.attributes.flatMap(att =>
      Java(s"private ${attrTypeGenerator(finalInterfaceName, att.tpe)} ${att.name};").fieldDeclarations())

    val params:Seq[String] = exp.attributes.map(att =>
      s"${attrTypeGenerator(finalInterfaceName, att.tpe)} ${att.name}")
    val getters: Seq[MethodDeclaration] =
      exp.attributes.flatMap(att => Java(s"""|public ${attrTypeGenerator(finalInterfaceName, att.tpe)} get${att.name.capitalize}() {
                                             |    return this.${att.name};
                                             |}""".stripMargin).methodDeclarations())
    val cons:Seq[Statement] = exp.attributes.flatMap(att => Java(s"  this.${att.name} = ${att.name};").statements())

    val constructor = Java(s"""|public $name (${params.mkString(",")}) {
                               |   ${cons.mkString("\n")}
                               |}""".stripMargin).constructors().head

    val interfaces = finalInterfaceName +: domain.lastModelWithOperation().ops.map(op => interfaceName(exp, op))

    Java(s"""
            |package expression;
            |public class $name implements ${interfaces.mkString(",")} {
            |
            |  ${constructor.toString}
            |
            |  ${getters.mkString("\n")}
            |  ${atts.mkString("\n")}
            |}""".stripMargin).compilationUnit()
   }

  def interfaceName(exp: domain.expressions.Exp, op: Operation): SimpleName = {
    Java(s"${exp.name}${op.name.capitalize}").simpleName()
  }

  override def methodGenerator(exp: domain.expressions.Exp)(op: domain.Operation): MethodDeclaration = {
    val method = super.methodGenerator(exp)(op)
    method.setDefault(true)
    method.setType(
      op.returnType match {
        case Some(tpe) => attrTypeGenerator(interfaceName(exp, op), tpe)
        case _ => Java("void").tpe
      })
    method.setModifier(Modifier.PUBLIC, false)
    method
  }


  def generateInterface(exp: domain.expressions.Exp, parents: Seq[SimpleName], op:Operation): CompilationUnit = {
    val name = interfaceName(exp, op)
    val method: MethodDeclaration = methodGenerator(exp)(op)
    val atts:Seq[MethodDeclaration] =
      exp.attributes.flatMap(att => Java(s"${attrTypeGenerator(baseInterfaceName(op), att.tpe)} get${att.name.capitalize}();").methodDeclarations())

    Java(s"""
            |package expression;
            |public interface $name extends ${parents.mkString(", ")} {
            |
            |  ${atts.mkString("\n")}
            |
            |  ${method}
            |}""".stripMargin).compilationUnit()
  }

  def finalInterfaceName: SimpleName = Java("FinalI").simpleName()

  def generateInterfaces(model: Model): Seq[CompilationUnit] = {
    val flat = model.flat()

    def generate(model: Model): Seq[CompilationUnit] = {
      val lastWithOps = model.last.lastModelWithOperation()
      val parents: Seq[SimpleName] =
        if (lastWithOps.isEmpty) Seq(Java(s"${typeGenerator(types.Exp)}").simpleName())
        else lastWithOps.ops.map(op => baseInterfaceName(op))

      def parentsFor(exp: domain.expressions.Exp): Seq[SimpleName] =
        if (lastWithOps.isEmpty) Seq.empty
        else lastWithOps.ops.map(op => interfaceName(exp, op))

      val parentUnits: Seq[CompilationUnit] = if (lastWithOps.isEmpty) Seq.empty else generate(lastWithOps)
      val newUnits: Seq[CompilationUnit] =
        model.ops.flatMap(op => generateBaseInterface(op, parents) +:
          flat.types.map(exp => generateInterface(exp, baseInterfaceName(op) +: parentsFor(exp), op)))

      parentUnits ++ newUnits
    }
    val lastWithOps = model.lastModelWithOperation()
    val finalParents: Seq[SimpleName] =
      if (lastWithOps.isEmpty) Seq(Java(s"${typeGenerator(types.Exp)}").simpleName())
      else lastWithOps.ops.map(op => baseInterfaceName(op))
    val finalInterface =
      Java(
        s"""
           |package expression;
           |public interface ${finalInterfaceName} extends ${finalParents.mkString(",")} {}
         """.stripMargin).compilationUnit()
    finalInterface +: generate(model)
  }

  def generateBaseInterface(op: Operation, parents: Seq[SimpleName]): CompilationUnit = {

    val retType = op.returnType match {
      case Some(tpe) => attrTypeGenerator(baseInterfaceName(op), tpe)
      case _ => Java("void").tpe
    }

    val params:String = op.parameters.map(tuple => {
      val name:String = tuple._1
      val tpe:types.Types = tuple._2

      typeGenerator(tpe).toString + " " + name
    }).mkString(",")


    val methodSignature: MethodDeclaration =
      Java(s"""public $retType ${op.name}($params);""").methodDeclarations().head

    Java(
      s"""package expression;
         |
         |public interface ${baseInterfaceName(op)} extends ${parents.mkString(", ")} {
         |    ${methodSignature}
         |}
       """.stripMargin).compilationUnit()
  }


  override def generateBase(domain: Model): CompilationUnit = {
    Java(
      s"""package expression;
         |
         |public interface ${typeGenerator(types.Exp)} {
         |}
       """.stripMargin).compilationUnit()
  }
}
