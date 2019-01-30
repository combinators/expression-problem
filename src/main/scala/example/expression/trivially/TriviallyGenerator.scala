package example.expression.trivially  /*DI:LD:AD*/

import com.github.javaparser.ast.Modifier
import com.github.javaparser.ast.body.{BodyDeclaration, MethodDeclaration}
import expression.ReplaceType
import org.combinators.templating.twirl.Java

trait TriviallyGenerator extends example.expression.oo.OOGenerator {

  /**
    * Generating "Expression problem, trivially" we need a class for each sub-type in model, then
    * an interface for all subtypes.
    * @return
    */
  override def generatedCode():Seq[CompilationUnit] = {
    val model = getModel
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

    decls ++ flat.types.map(tpe => generateExp(model, tpe)) ++     // one class for each sub-type
      generateInterfaces(model) :+                        // interfaces for all subtypes
      generateBase(model)                                 // base interface
  }

  /**
    * For producer operations, there is a need to instantiate objects, and one would use this
    * method (with specific parameters) to carry this out.
    *
    * For recursive types, use "FinalI" as the cast internally, otherwise use native type
    */
  override def inst(exp:domain.Atomic, params:Expression*): Expression = {

    val merged = exp.attributes.map(att => att.tpe).zip(params).map(typeExp => {
      val tpe:domain.TypeRep = typeExp._1
      val inner:Expression = typeExp._2
      tpe match {
        case domain.baseTypeRep => Java(s"""(FinalI)($inner)""").expression()
        case _ => inner
      }
    })
    Java("new " + exp.concept + "(" + merged.map(expr => expr.toString()).mkString(",") + ")").expression()
  }

  /**
    * Retrieve expression by getXXX accessor method.
    */
  override def expression (exp:domain.Atomic, att:domain.Attribute) : Expression = {
    Java(s"get${att.concept}()").expression()
  }

  /** Handle self-case here. */
  override def contextDispatch(source:Context, delta:Delta) : Expression = {
    if (delta.expr.isEmpty) {
      val op = delta.op.get.instance
      val args:String = delta.params.mkString(",")
      Java(s"this.$op($args)").expression()
    } else {
      super.contextDispatch(source, delta)
    }
  }

  def baseInterfaceName(op: domain.Operation): Type = {
    Java(s"${domain.baseTypeRep.concept}${op.concept}").tpe()
  }

  // Needs covariant overriding!
  override def generateExp(model:domain.Model, exp:domain.Atomic) : CompilationUnit = {
    val name = Java(s"${exp.concept}").simpleName()

    val interfaces = finalInterfaceName +: model.lastModelWithOperation().ops.map(op => interfaceName(exp, op))
    val newType:com.github.javaparser.ast.`type`.Type = Java(finalInterfaceName).tpe()

    val compUnit = Java(s"""
            |package trivially;
            |public class $name implements ${interfaces.mkString(",")} {
            |
            |  ${constructor(exp).toString}
            |
            |  ${getters(exp).mkString("\n")}
            |  ${fields(exp).mkString("\n")}
            |}""".stripMargin).compilationUnit()

    // replace all covariant types!
    ReplaceType.replace(compUnit, Java(s"${domain.baseTypeRep.concept}").tpe, finalInterfaceName)

    compUnit
   }

  def interfaceName(exp: domain.Atomic, op: domain.Operation): Type = {
    Java(s"${exp.concept}${op.concept}").tpe()
  }

  override def methodGenerator(exp: domain.Atomic, op: domain.Operation): MethodDeclaration = {
    val method = super.methodGenerator(exp, op)
    method.setDefault(true)
    method.setType(
      op.returnType match {
        case Some(domain.baseTypeRep) => typeConverter(domain.baseTypeRep)
        case Some(tpe) => typeConverter(tpe) // , Some(interfaceName(exp, op)))
        case _ => Java("void").tpe
      })

    method.setModifier(Modifier.PUBLIC, false)


    // replace all types!
    ReplaceType.replace(method, Java(s"${domain.baseTypeRep.concept}").tpe,
      Java(domain.baseTypeRep.concept + op.concept).tpe())

    method
  }

  def generateInterface(exp: domain.Atomic, parents: Seq[Type], op:domain.Operation): CompilationUnit = {
    val name = interfaceName(exp, op)
    val method: MethodDeclaration = methodGenerator(exp, op)
    val atts:Seq[MethodDeclaration] =
      exp.attributes.flatMap(att => Java(s"${typeConverter(att.tpe)} get${att.concept}();").methodDeclarations())

    val unit = Java(s"""
            |package trivially;
            |public interface $name extends ${parents.mkString(", ")} {
            |
            |  ${atts.mkString("\n")}
            |  $method
            |}""".stripMargin).compilationUnit()


    ReplaceType.replace(unit, Java(s"${domain.baseTypeRep.concept}").tpe, baseInterfaceName(op))

    unit
  }

  def finalInterfaceName: Type = Java("FinalI").tpe()

  def generateInterfaces(model: domain.Model): Seq[CompilationUnit] = {
    val flat = model.flatten()

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
      case Some(tpe) => typeConverter(tpe)
      case _ => Java("void").tpe
    }

    val params:String = op.parameters.map(param => typeConverter(param.tpe).toString + " " + param.name).mkString(",")

    val methodSignature: MethodDeclaration =
      Java(s"""public $retType ${op.instance}($params);""").methodDeclarations().head

    val compUnit = Java(s"""
         |package trivially;
         |
         |public interface ${baseInterfaceName(op)} extends ${parents.mkString(", ")} {
         |
         |    $methodSignature
         |}
       """.stripMargin).compilationUnit()

    // replace all types!
    ReplaceType.replace(compUnit, Java(s"${domain.baseTypeRep.concept}").tpe, baseInterfaceName(op))

    compUnit
  }

  override def generateBase(model: domain.Model): CompilationUnit = {

    val binaryMethodHelper: Seq[BodyDeclaration[_]] = if (model.flatten().ops.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      Java(s"""public tree.Tree ${domain.AsTree.instance}();""").classBodyDeclarations
    } else {
      Seq.empty
    }

    Java(
      s"""package trivially;
         |
         |public interface ${typeConverter(domain.baseTypeRep)} {
         |    ${binaryMethodHelper.mkString("\n")}
         |}
       """.stripMargin).compilationUnit()
  }
}
