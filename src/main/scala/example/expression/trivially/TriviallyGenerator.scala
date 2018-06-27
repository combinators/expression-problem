package example.expression.trivially

import com.github.javaparser.ast.{CompilationUnit, Modifier}
import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.body.{FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr.{Expression, NameExpr, SimpleName}
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.Domain
import example.expression.j.{AbstractGenerator, DataTypeSubclassGenerator}
import org.combinators.templating.twirl.Java

trait TriviallyGenerator extends example.expression.oo.StraightGenerator {
 // import domain._

//  /**
//    * Must eliminate any operation that returns E as value, since can't handle Producer methods
//    */
//  override def compatible(model:Model):Model = {
//    if (model.isEmpty) { return model }
//
//    // rebuild by filtering out all operations that return Exp.
//    Model (model.name, model.types, model.ops.filterNot(op => op.returnType.isDefined && op.returnType.get.equals(types.Exp)), compatible(model.last))
//  }

  /**
    * For producer operations, there is a need to instantiate objects, and one would use this
    * method (with specific parameters) to carry this out.
    *
    * For recursive types, use "FinalI" as the cast internally, otherwise use native type
    */
  override def inst(exp:domain.expressions.Exp)(op:domain.Operation)(params:Expression*): Expression = {
    // seq1 zip seq2
    val merged:Seq[Expression] = exp.attributes.map(att => att.tpe).zip(params).map(typeExp => {
      val x:domain.types.Types = typeExp._1
      val inner:Expression = typeExp._2
      x match {
        case domain.Exp => Java(s"""(FinalI)($inner)""").expression[Expression]()
        case _ => inner
      }
    })
    Java("new " + exp.name + "(" + merged.map(expr => expr.toString()).mkString(",") + ")").expression()
  }

  override def subExpressions(exp: domain.expressions.Exp): Map[String, Expression] = {
    exp.attributes.map(att => att.name -> Java(s"get${att.name.capitalize}()").expression[Expression]()).toMap
  }

  // note: this is very much like recursiveTypeGenerator in other generators. come up with standard name
  def attrTypeGenerator(currentClass: SimpleName, tpe: domain.types.Types): Type = {
    tpe match {
      case domain.Exp => Java(s"$currentClass").tpe()
      case _ => typeGenerator(tpe)
    }
  }

  def baseInterfaceName(op: domain.Operation): SimpleName = {
    Java(s"Exp${op.name.capitalize}").simpleName()
  }

  override def generateExp(model:domain.Model, exp:domain.expressions.Exp) : CompilationUnit = {
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

    val interfaces = finalInterfaceName +: model.lastModelWithOperation().ops.map(op => interfaceName(exp, op))

    Java(s"""
            |package trivially;
            |public class $name implements ${interfaces.mkString(",")} {
            |
            |  ${constructor.toString}
            |
            |  ${getters.mkString("\n")}
            |  ${atts.mkString("\n")}
            |}""".stripMargin).compilationUnit()
   }

  def interfaceName(exp: domain.expressions.Exp, op: domain.Operation): SimpleName = {
    Java(s"${exp.name}${op.name.capitalize}").simpleName()
  }

  override def methodGenerator(exp: domain.expressions.Exp)(op: domain.Operation): MethodDeclaration = {
    val method = super.methodGenerator(exp)(op)
    method.setDefault(true)
    method.setType(
      op.returnType match {
        case Some(domain.Exp) => attrTypeGenerator(Java("Exp" + op.name.capitalize).simpleName(), domain.Exp)  // producers... HEINEMAN
        case Some(tpe) => attrTypeGenerator(interfaceName(exp, op), tpe)
        case _ => Java("void").tpe
      })
    method.setModifier(Modifier.PUBLIC, false)
    method
  }


  def generateInterface(exp: domain.expressions.Exp, parents: Seq[SimpleName], op:domain.Operation): CompilationUnit = {
    val name = interfaceName(exp, op)
    val method: MethodDeclaration = methodGenerator(exp)(op)
    val atts:Seq[MethodDeclaration] =
      exp.attributes.flatMap(att => Java(s"${attrTypeGenerator(baseInterfaceName(op), att.tpe)} get${att.name.capitalize}();").methodDeclarations())

    Java(s"""
            |package trivially;
            |public interface $name extends ${parents.mkString(", ")} {
            |
            |  ${atts.mkString("\n")}
            |
            |  $method
            |}""".stripMargin).compilationUnit()
  }

  def finalInterfaceName: SimpleName = Java("FinalI").simpleName()

  def generateInterfaces(model: domain.Model): Seq[CompilationUnit] = {
    val flat = model.flat()

    def generate(model: domain.Model): Seq[CompilationUnit] = {
      val lastWithOps = model.last.lastModelWithOperation()
      val parents: Seq[SimpleName] =
        if (lastWithOps.isEmpty) Seq(Java(s"${typeGenerator(domain.Exp)}").simpleName())
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
      if (lastWithOps.isEmpty) Seq(Java(s"${typeGenerator(domain.Exp)}").simpleName())
      else lastWithOps.ops.map(op => baseInterfaceName(op))
    val finalInterface =
      Java(
        s"""
           |package trivially;
           |public interface ${finalInterfaceName} extends ${finalParents.mkString(",")} {}
         """.stripMargin).compilationUnit()
    finalInterface +: generate(model)
  }

  def generateBaseInterface(op: domain.Operation, parents: Seq[SimpleName]): CompilationUnit = {

    val retType = op.returnType match {
      case Some(tpe) => attrTypeGenerator(baseInterfaceName(op), tpe)
      case _ => Java("void").tpe
    }

    val params:String = op.parameters.map(tuple => {
      val name:String = tuple._1
      val tpe:domain.types.Types = tuple._2

      typeGenerator(tpe).toString + " " + name
    }).mkString(",")


    val methodSignature: MethodDeclaration =
      Java(s"""public $retType ${op.name}($params);""").methodDeclarations().head

    Java(
      s"""package trivially;
         |
         |public interface ${baseInterfaceName(op)} extends ${parents.mkString(", ")} {
         |    ${methodSignature}
         |}
       """.stripMargin).compilationUnit()
  }


  override def generateBase(model: domain.Model): CompilationUnit = {
    Java(
      s"""package trivially;
         |
         |public interface ${typeGenerator(domain.Exp)} {
         |}
       """.stripMargin).compilationUnit()
  }
}
