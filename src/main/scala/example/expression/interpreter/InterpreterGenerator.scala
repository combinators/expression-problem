package example.expression.interpreter  /*DI:LD:AD*/

import com.github.javaparser.ast.body.{FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr.SimpleName
import example.expression.j._
import expression.ReplaceType
import org.combinators.templating.twirl.Java

trait InterpreterGenerator extends JavaGenerator with DataTypeSubclassGenerator  with OperationAsMethodGenerator with JavaBinaryMethod {

  /**
    * Generating an interpreter solution requires:
    *
    * 1. A Class for every data type
    * 2. A Class for every operation
    * 3. Abstract Base class and visitor class
    * @return
    */
  def generatedCode():Seq[CompilationUnit] = {
    val model = getModel

    //  binary methods for helper
    val decls:Seq[CompilationUnit] = if (model.flatten().ops.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      helperClasses()
    } else {
      Seq.empty
    }

    // one interface for every model that contains an operation
    // Each operation gets interface
    decls ++ model.inChronologicalOrder.filter(m => m.ops.nonEmpty).map(m => generateBase(m)) ++
    //
    // Each operation must provide class implementations for all past dataTypes since last operation
    model.inChronologicalOrder.filter(m => m.ops.nonEmpty).flatMap(m => generateBaseExtensions(m)) ++
    generateIntermediateTypes(model)
  }

  /**
    * For producer operations, there is a need to instantiate objects, and one would use this
    * method (with specific parameters) to carry this out.
    *
    * For interpreter, we use a factory method that has been placed in the class, and that allows
    * the very specialized types to be used.
    */
  override def inst(exp:domain.Atomic, params:Expression*): Expression = {
    Java(exp.concept + "(" + params.map(expr => expr.toString()).mkString(",") + ")").expression()
  }

  override def expression (exp:domain.Atomic, att:domain.Attribute) : Expression = {
    Java(s"get${att.concept}()").expression()
  }

  /** Return designated Java type associated with type, or void if all else fails. */
  override def typeConverter(tpe:domain.TypeRep) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case domain.baseTypeRep => Java(s"${domain.baseTypeRep.concept}").tpe()
      case _ => super.typeConverter(tpe)
    }
  }

  /** Handle self-case here. */
  override def contextDispatch(source:Context, delta:Delta) : Expression = {
    if (delta.expr.isEmpty) {
      val op = delta.op.get.instance
      val args = delta.params.mkString(",")
      Java(s"this.$op($args)").expression()
    } else {
      super.contextDispatch(source, delta)
    }
  }

  /** Directly access local method, one per operation, with a parameter. */
  override def dispatch(expr:Expression, op:domain.Operation, params:Expression*) : Expression = {
    val args:String = params.mkString(",")
    Java(s"""$expr.${op.instance}($args)""").expression()
  }

  def modelInterfaceName(model:domain.Model): String = {
    model.ops.sortWith(_.name < _.name).map(op => op.concept).mkString("") + "Exp"
  }

  /** Find Model with operations and return that one. */
  def baseInterfaceName(m:domain.Model): SimpleName = {
      Java(m.lastModelWithOperation().ops.sortWith(_.name < _.name).map(op => op.concept).mkString("") + "Exp").simpleName()
  }

  /** Operations are implemented as methods in the Base and sub-type classes. */
  override def methodGenerator(exp:domain.Atomic, op:domain.Operation): MethodDeclaration = {
    val retType = op.returnType match {
      case Some(tpe) => typeConverter(tpe)
      case _ => Java("void").tpe
    }

    val params = parameters(op)
    Java(s"""|public $retType ${op.instance}($params) {
             |  ${logic(exp, op).mkString("\n")}
             |}""".stripMargin).methodDeclarations().head
  }

  /**
    * Must extend base EvalExp
    * Must have constructor and access
    * Must have operation methods
    *
    * @param model
    * @param exp
    * @return
    */
  override def generateExp(model:domain.Model, exp:domain.Atomic) : CompilationUnit = {
    val name = Java(s"${exp.concept}").simpleName()
    val baseInterface:Option[Type] = Some(Java(baseInterfaceName(model.lastModelWithOperation())).tpe())

    // provide method declarations for all past operations (including self). But if we extend, can't we stop at last op?
    val allOps:Seq[domain.Operation] = model.pastOperations()
    val operations:Seq[MethodDeclaration] = allOps.map(op => methodGenerator(exp,op))

    val unit = Java(s"""
            |package interpreter;
            |public class $name implements ${baseInterface.toString} {
            |
            |  ${constructor(exp).toString}
            |
            |  ${getters(exp).mkString("\n")}
            |  ${fields(exp).mkString("\n")}
            |  ${operations.mkString("\n")}
            |}""".stripMargin).compilationUnit()

    // replace all covariant types!
    ReplaceType.replace(unit, Java(s"${domain.baseTypeRep.concept}").tpe, baseInterface.get)

    unit
   }

  def interfaceName(exp: domain.Atomic, op: domain.Operation): SimpleName = {
    Java(s"${exp.concept}${op.concept}").simpleName()
  }

  def generateInterface(exp: domain.Atomic, parents: Seq[SimpleName], op:domain.Operation): CompilationUnit = {
    val name = interfaceName(exp, op)
    val method: MethodDeclaration = methodGenerator(exp, op)
    val atts:Seq[MethodDeclaration] =
      exp.attributes.flatMap(att => Java(s"${typeConverter(att.tpe)} get${att.concept}();").methodDeclarations())

    Java(s"""
            |package interpreter;
            |public interface $name extends ${parents.mkString(", ")} {
            |
            |  ${atts.mkString("\n")}
            |
            |  $method
            |}""".stripMargin).compilationUnit()
  }

  def finalInterfaceName: SimpleName = Java("FinalI").simpleName()

  /**
    * Generate one interface for model that defines an operation. If two or more operations
    * are defined in the same model, then concatenate together.
    *
    * Shall only be called on a model with operations.
    *
    * Added special logic for BinaryMethod
    *
    * @param model
    * @return
    */
  override def generateBase(model: domain.Model): CompilationUnit = {
    // concatenate all names
    val fullType:Type = Java(modelInterfaceName(model)).tpe()
    val signatures:Seq[String] = model.ops.map(op => {

      val params: Seq[String] = op.parameters.map(tuple => {
        val name = tuple._1
        val tpe = tuple._2

        op match {
          case bm:domain.BinaryMethod => if (tpe.equals(domain.baseTypeRep)) {
            s"$fullType $name"
          } else {
            s"${typeConverter(tpe)} $name"
          }
          case _ => s"${typeConverter(tpe)} $name"
        }
      })

      op.returnType.get match {
        case domain.baseTypeRep => s"""public $fullType ${op.instance}(${params.mkString(",")});"""
        case _ => s"""public ${typeConverter(op.returnType.get)}  ${op.instance}(${params.mkString(",")});"""
      }
    })

    // see if we are first.
    val lastWithOps = if (model.ops.isEmpty) { model.lastModelWithOperation() }
    else { model.last.lastModelWithOperation()}

    val extension:String = if (lastWithOps.isEmpty) ""
      else "extends " + modelInterfaceName(lastWithOps)

    Java(s"""|package interpreter;
             |public interface ${fullType.toString} $extension {
             |  ${signatures.mkString("\n")}
             |}""".stripMargin).compilationUnit
  }

  def lastTypesSinceAnOperation(model:domain.Model): Seq[domain.Atomic] = {
    if (model.isEmpty || model.ops.nonEmpty) {
      Seq.empty
    } else {
      model.types ++ lastTypesSinceAnOperation(model.last)
    }
  }

  /**
    * For all new types added since last operation, need to provide implementation.
    *
    * Note: will never be base, so pass in false
    * @param model
    * @return
    */
  def generateIntermediateTypes(model:domain.Model): Seq[CompilationUnit] = {
    if (model.isEmpty) { return Seq.empty }
    if (model.ops.nonEmpty) { return generateIntermediateTypes(model.last) }   // continue onwards.

    // compute new types since last operation

    val last = model.lastModelWithOperation()   // HACK. true is not best ansewr
    generateForOp(model, last.ops, lastTypesSinceAnOperation(model), isBase=true) ++ generateIntermediateTypes(model.last)
  }

  // if multiple operations in the same model, then must chain together.
  def generateBaseExtensions(model:domain.Model) : Seq[CompilationUnit] = {
    val pastTypes:Seq[domain.Atomic] = model.pastDataTypes()

    val isBase:Boolean = model.base().equals(model)

    generateForOp(model, model.ops, pastTypes, isBase)
  }

  /**
    * For each operation must generate a sequence of classes, one per subtype.
    *
    * Note that BinaryMethods must have their Exp parameters converted to be \${op.name}Exp.
     */
  def generateForOp(model:domain.Model, ops:Seq[domain.Operation], pastTypes:Seq[domain.Atomic], isBase:Boolean) : Seq[CompilationUnit] = {
    val combinedOps:String = ops.sortWith(_.name < _.name).map(op => op.concept).mkString("")

      pastTypes.map(exp => {
        val name = Java(s"${exp.concept}").simpleName()
        val baseInterface:Type = Java(baseInterfaceName(model.lastModelWithOperation())).tpe()

        val atts:Seq[FieldDeclaration] = if (isBase) {
          exp.attributes.flatMap(att => Java(s"${typeConverter(att.tpe)} ${att.instance};").fieldDeclarations())
        } else {
          Seq.empty
        }

        val params:Seq[String] = exp.attributes.map(att => s"${typeConverter(att.tpe)} ${att.instance}")
        val paramNames:Seq[String] = exp.attributes.map(att => s"${att.instance}")

        val factoryMethods:Seq[MethodDeclaration] = pastTypes.flatMap(e => {
          val params:Seq[String] = e.attributes.map(att => s"${typeConverter(att.tpe)} ${att.instance}")
          val paramNames:Seq[String] = e.attributes.map(att => s"${att.instance}")

          Java(s"""${combinedOps}Exp ${e.concept}(${params.mkString(",")}) { return new $combinedOps${e.concept}(${paramNames.mkString(",")}); }""").methodDeclarations()
        })

        val getters: Seq[MethodDeclaration] =
          exp.attributes.flatMap(att => {
            // anything that is an EXPR can be replaced
            val cast = att.tpe match {
              case domain.baseTypeRep => if (!isBase) { s"($baseInterface)" } else { "" }
              case _ => ""
            }

            Java(s"""|public ${typeConverter(att.tpe)} get${att.concept}() {
                     |    return $cast this.${att.instance};
                     |}""".stripMargin).methodDeclarations()
          })

        val cons:Seq[Statement] = exp.attributes.flatMap(att => Java(s"  this.${att.instance} = ${att.instance};").statements())

        val constructor = if (isBase) {
          Java(s"""|public $combinedOps$name (${params.mkString(",")}) {
                   |   ${cons.mkString("\n")}
                   |}""".stripMargin).constructors().head
        } else {
          Java(s"""|public $combinedOps$name (${params.mkString(",")}) {
                   |   super(${paramNames.mkString(",")});
                   |}""".stripMargin).constructors().head
        }

        // provide method declarations for all past operations (including self) that are not already in our 'ops' set.
        val allOps:Seq[domain.Operation] = model.pastOperations().filterNot(op => ops.contains(op))

        // be sure to recursively change Exp to be fullType
        val fullType:Type = Java(modelInterfaceName(model)).tpe()

        // i.e., without this, you get (M4) PrettyPAdd and PrettyPSub both having eval method
        val operations:Seq[MethodDeclaration] = (allOps ++ ops).flatMap(op => {

          // can remove operations that already existed model.last.lastModelWithOperation().ops
          // if exp existed PRIOR to model.last.lastModelWithOperation().ops than can omit
          // must ensure op is not past of *this* model's newly added ops since those MUST appear.
          if (!ops.contains(op) && model.lastModelWithOperation().flatten().types.contains(exp)) {
            Seq.empty
          } else {
            val md: MethodDeclaration = methodGenerator(exp, op)

            // be sure to recursively change any Exp into fullType, for producer capability
            val returnType: Type = md.getType
            if (returnType.equals(typeConverter(domain.baseTypeRep))) {
              md.setType(fullType)
            }

            // same thing for parameters
            md.getParameters.forEach(p => {
              if (p.getType.equals(typeConverter(domain.baseTypeRep))) {
                p.setType(fullType)
              }
            })

            Seq(md)
          }
        })

        // For this operation (i.e., "Print") and for this Exp (i.e., "Add") go back and find most recent class to extend (i.e., "EvalAdd")
        // which is based on either (a) last operation
        // when multiple operations defined in same model, then chain together arbitrarily
        val extension:String =
          if (isBase) {
            ""
          } else {
            val past: String = model.last.lastModelWithOperation().ops.sortWith(_.name < _.name).map(op => op.concept).mkString("")
            s"extends $past$name" // go backwards?
          }

        val unit = Java(s"""
                |package interpreter;
                |public class $combinedOps$name $extension implements ${baseInterface.toString} {
                |
                |  ${factoryMethods.mkString("\n")}
                |  ${constructor.toString}
                |
                |  ${getters.mkString("\n")}
                |  ${atts.mkString("\n")}
                |  ${operations.mkString("\n")}
                |}""".stripMargin).compilationUnit()

        // replace all covariant types!
        ReplaceType.replace(unit, Java(s"${domain.baseTypeRep.concept}").tpe, baseInterface)

        unit
      })
  }
}
