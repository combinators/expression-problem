package example.expression.trivially

import com.github.javaparser.ast.{CompilationUnit, Modifier}
import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.body.{FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.j.Producer
import org.combinators.templating.twirl.Java

trait TriviallyGenerator extends example.expression.oo.OOGenerator with Producer {

  /**
    * Must eliminate any operation that returns E as value, since Algebra doesn't instantiate the intermediate structures
    */
  override def apply(model:domain.Model):domain.Model = {
    if (model.isEmpty) { return model }

    // rebuild by filtering out all ProducerOperations
    domain.Model(model.name, model.types,
      model.ops.filterNot(op => op.isInstanceOf[domain.ProducerOperation]),
      apply(model.last))
  }

  /**
    * Generating "Expression problem, trivially" we need a class for each sub-type in model, then
    * an interface for all subtypes.
    * @param model
    * @return
    */
  override def generatedCode(model:domain.Model):Seq[CompilationUnit] = {
    // flatten hierarchy and remove producer operations (i.e., those that are not compatible with this approach)

    val flat = model.flat()
    flat.types.map(tpe => generateExp(model, tpe)) ++     // one class for each sub-type
      generateInterfaces(model) :+                        // interfaces for all subtypes
      generateBase(model)                                 // base  interface
  }

  /**
    * For producer operations, there is a need to instantiate objects, and one would use this
    * method (with specific parameters) to carry this out.
    *
    * For recursive types, use "FinalI" as the cast internally, otherwise use native type
    */
  override def inst(exp:domain.Atomic)(op:domain.Operation)(params:Expression*): Expression = {

    val merged:Seq[Expression] = exp.attributes.map(att => att.tpe).zip(params).map(typeExp => {
      val tpe:domain.TypeRep = typeExp._1
      val inner:Expression = typeExp._2
      tpe match {
        case domain.baseTypeRep => Java(s"""(FinalI)($inner)""").expression[Expression]()
        case _ => inner
      }
    })
    Java("new " + exp.name + "(" + merged.map(expr => expr.toString()).mkString(",") + ")").expression()
  }

  override def subExpressions(exp: domain.Atomic): Map[String, Expression] = {
    exp.attributes.map(att => att.name -> Java(s"get${att.name.capitalize}()").expression[Expression]()).toMap
  }

  def baseInterfaceName(op: domain.Operation): Type = {
    Java(s"Exp${op.name.capitalize}").tpe()
  }

  override def generateExp(model:domain.Model, exp:domain.Atomic) : CompilationUnit = {
    val name = Java(s"${exp.name}").simpleName()

    val fi = finalInterfaceName

    val atts:Seq[FieldDeclaration] = exp.attributes.flatMap(att =>
      Java(s"private ${typeConverter(att.tpe, Some(finalInterfaceName))} ${att.name};").fieldDeclarations())

    val params:Seq[String] = exp.attributes.map(att =>
      s"${typeConverter(att.tpe, Some(finalInterfaceName))} ${att.name}")
    val getters: Seq[MethodDeclaration] =
      exp.attributes.flatMap(att => Java(s"""|public ${typeConverter(att.tpe, Some(finalInterfaceName))} get${att.name.capitalize}() {
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

  def interfaceName(exp: domain.Atomic, op: domain.Operation): Type = {
    Java(s"${exp.name}${op.name.capitalize}").tpe()
  }

  override def methodGenerator(exp: domain.Atomic)(op: domain.Operation): MethodDeclaration = {
    val method = super.methodGenerator(exp)(op)
    method.setDefault(true)
    method.setType(
      op.returnType match {
        case Some(domain.baseTypeRep) => typeConverter(domain.baseTypeRep, Some(Java("Exp" + op.name.capitalize).tpe()))
        case Some(tpe) => typeConverter(tpe, Some(interfaceName(exp, op)))
        case _ => Java("void").tpe
      })

    method.setModifier(Modifier.PUBLIC, false)
    method
  }


  def generateInterface(exp: domain.Atomic, parents: Seq[Type], op:domain.Operation): CompilationUnit = {
    val name = interfaceName(exp, op)
    val method: MethodDeclaration = methodGenerator(exp)(op)
    val atts:Seq[MethodDeclaration] =
      exp.attributes.flatMap(att => Java(s"${typeConverter(att.tpe, Some(baseInterfaceName(op)))} get${att.name.capitalize}();").methodDeclarations())

    Java(s"""
            |package trivially;
            |public interface $name extends ${parents.mkString(", ")} {
            |
            |  ${atts.mkString("\n")}
            |
            |  $method
            |}""".stripMargin).compilationUnit()
  }

  def finalInterfaceName: Type = Java("FinalI").tpe()

  def generateInterfaces(model: domain.Model): Seq[CompilationUnit] = {
    val flat = model.flat()

    def generate(model: domain.Model): Seq[CompilationUnit] = {
      val lastWithOps = model.last.lastModelWithOperation()
      val parents: Seq[Type] =
        if (lastWithOps.isEmpty) Seq(Java(s"${typeConverter(domain.baseTypeRep)}").tpe())
        else lastWithOps.ops.map(op => baseInterfaceName(op))

      def parentsFor(exp: domain.Atomic): Seq[Type] =
        if (lastWithOps.isEmpty) Seq.empty
        else lastWithOps.ops.map(op => interfaceName(exp, op))

      val parentUnits: Seq[CompilationUnit] = if (lastWithOps.isEmpty) Seq.empty else generate(lastWithOps)
      val newUnits: Seq[CompilationUnit] =
        model.ops.flatMap(op => generateBaseInterface(op, parents) +:
          flat.types.map(exp => generateInterface(exp, baseInterfaceName(op) +: parentsFor(exp), op)))

      parentUnits ++ newUnits
    }
    val lastWithOps = model.lastModelWithOperation()
    val finalParents: Seq[Type] =
      if (lastWithOps.isEmpty) Seq(Java(s"${typeConverter(domain.baseTypeRep)}").tpe())
      else lastWithOps.ops.map(op => baseInterfaceName(op))
    val finalInterface =
      Java(
        s"""
           |package trivially;
           |public interface $finalInterfaceName extends ${finalParents.mkString(",")} {}
         """.stripMargin).compilationUnit()
    finalInterface +: generate(model)
  }

  def generateBaseInterface(op: domain.Operation, parents: Seq[Type]): CompilationUnit = {

    val retType = op.returnType match {
      case Some(tpe) => typeConverter(tpe, Some(baseInterfaceName(op)))
      case _ => Java("void").tpe
    }

    val params:String = op.parameters.map(tuple => {
      val name:String = tuple._1
      val tpe:domain.TypeRep = tuple._2

      typeConverter(tpe).toString + " " + name
    }).mkString(",")


    val methodSignature: MethodDeclaration =
      Java(s"""public $retType ${op.name}($params);""").methodDeclarations().head

    Java(
      s"""package trivially;
         |
         |public interface ${baseInterfaceName(op)} extends ${parents.mkString(", ")} {
         |    $methodSignature
         |}
       """.stripMargin).compilationUnit()
  }


  override def generateBase(model: domain.Model): CompilationUnit = {
    Java(
      s"""package trivially;
         |
         |public interface ${typeConverter(domain.baseTypeRep)} {
         |}
       """.stripMargin).compilationUnit()
  }
}
