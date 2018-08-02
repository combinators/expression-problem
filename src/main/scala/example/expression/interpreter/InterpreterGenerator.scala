package example.expression.interpreter  /*DI:LD:AD*/

import com.github.javaparser.ast.body.{BodyDeclaration, FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr.SimpleName
import example.expression.j._
import org.combinators.templating.twirl.Java

trait InterpreterGenerator extends  AbstractGenerator with DataTypeSubclassGenerator with StandardJavaBinaryMethod with OperationAsMethodGenerator with JavaBinaryMethod with Producer {

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
    // one interface for every model that contains an operation
    // Each operation gets interface
    model.inChronologicalOrder.filter(m => m.ops.nonEmpty).map(m => generateBase(m)) ++
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
  override def inst(exp:domain.Atomic)(op:domain.Operation)(params:Expression*): Expression = {
    Java(exp.name + "(" + params.map(expr => expr.toString()).mkString(",") + ")").expression()
  }

  override def subExpressions(exp: domain.Atomic): Map[String, Expression] = {
    exp.attributes.map(att => att.name -> Java(s"get${att.name.capitalize}()").expression[Expression]()).toMap
  }

  /** Return designated Java type associated with type, or void if all else fails. */
  override def typeConverter(tpe:domain.TypeRep, covariantReplacement:Option[Type] = None) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case domain.baseTypeRep => covariantReplacement.getOrElse(Java("Exp").tpe())
      case _ => super.typeConverter(tpe, covariantReplacement)
    }
  }

  /** Directly access local method, one per operation, with a parameter. */
  override def dispatch(expr:Expression, op:domain.Operation, params:Expression*) : Expression = {
    val args:String = params.mkString(",")
    Java(s"""$expr.${op.name}($args)""").expression()
  }

  def modelInterfaceName(model:domain.Model): String = {
    model.ops.sortWith(_.name < _.name).map(op => op.name.capitalize).mkString("") + "Exp"
  }

  /** Find Model with operations and return that one. */
  def baseInterfaceName(m:domain.Model): SimpleName = {
      Java(m.lastModelWithOperation().ops.sortWith(_.name < _.name).map(op => op.name.capitalize).mkString("") + "Exp").simpleName()
  }

  /** Operations are implemented as methods in the Base and sub-type classes. */
  override def methodGenerator(exp:domain.Atomic)(op:domain.Operation): MethodDeclaration = {
    val retType = op.returnType match {
      case Some(tpe) => typeConverter(tpe)
      case _ => Java("void").tpe
    }

    val params = parameters(op)
    Java(s"""|public $retType ${op.name}($params) {
             |  ${logic(exp)(op).mkString("\n")}
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
    val name = Java(s"${exp.name}").simpleName()
    val baseInterface:Option[Type] = Some(Java(baseInterfaceName(model.lastModelWithOperation())).tpe())

    // provide method declarations for all past operations (including self). But if we extend, can't we stop at last op?
    val allOps:Seq[domain.Operation] = model.pastOperations()
    val operations:Seq[MethodDeclaration] = allOps.map(op =>
        methodGenerator(exp)(op))

    Java(s"""
            |package interpreter;
            |public class $name implements ${baseInterface.toString} {
            |
            |  ${constructor(exp).toString}
            |
            |  ${getters(exp, baseInterface).mkString("\n")}
            |  ${fields(exp, baseInterface).mkString("\n")}
            |  ${operations.mkString("\n")}
            |}""".stripMargin).compilationUnit()
   }

  def interfaceName(exp: domain.Atomic, op: domain.Operation): SimpleName = {
    Java(s"${exp.name}${op.name.capitalize}").simpleName()
  }

  def generateInterface(exp: domain.Atomic, parents: Seq[SimpleName], op:domain.Operation): CompilationUnit = {
    val name = interfaceName(exp, op)
    val method: MethodDeclaration = methodGenerator(exp)(op)
    val atts:Seq[MethodDeclaration] =
      exp.attributes.flatMap(att => Java(s"${typeConverter(att.tpe)} get${att.name.capitalize}();").methodDeclarations())

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

      s"""public ${typeConverter(op.returnType.get, Some(fullType))}  ${op.name}(${params.mkString(",")});"""
    })

    // see if we are first.
    val lastWithOps = if (model.ops.isEmpty) { model.lastModelWithOperation() }
    else { model.last.lastModelWithOperation()}

    val extension:String = if (lastWithOps.isEmpty) ""
      else "extends " + modelInterfaceName(lastWithOps)

    // include helper methods for AsTree.
    val decls: Seq[BodyDeclaration[_]] = if (model.flatten().ops.exists {
      case bm: domain.BinaryMethodTreeBase => true
      case _ => false
    }) {
      declarations
    } else {
      Seq.empty
    }

    Java(s"""|package interpreter;
                  |public interface ${fullType.toString} $extension {
                  |  ${decls.mkString("\n")}
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
    //last.ops.flatMap(op => generateForOp(model, op, lastTypesSinceAnOperation(model), isBase=true)) ++ generateIntermediateTypes(model.last)
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
    * Note that BinaryMethods must have their Exp parameters converted to be ${op.name}Exp.
    *
    * @param model
    * @param ops
    * @param pastTypes
    * @param isBase
    * @return
    */
  def generateForOp(model:domain.Model, ops:Seq[domain.Operation], pastTypes:Seq[domain.Atomic], isBase:Boolean) : Seq[CompilationUnit] = {
    val combinedOps:String = ops.sortWith(_.name < _.name).map(op => op.name.capitalize).mkString("")

      pastTypes.map(exp => {
        val name = Java(s"${exp.name}").simpleName()
        val baseInterface:Type = Java(baseInterfaceName(model.lastModelWithOperation())).tpe()

        val atts:Seq[FieldDeclaration] = if (isBase) {
          exp.attributes.flatMap(att => Java(s"${typeConverter(att.tpe, Some(baseInterface))} ${att.name};").fieldDeclarations())
        } else {
          Seq.empty
        }

        val params:Seq[String] = exp.attributes.map(att => s"${typeConverter(att.tpe, Some(baseInterface))} ${att.name}")
        val paramNames:Seq[String] = exp.attributes.map(att => s"${att.name}")

        val factoryMethods:Seq[MethodDeclaration] = pastTypes.flatMap(e => {
          val params:Seq[String] = e.attributes.map(att => s"${typeConverter(att.tpe, Some(baseInterface))} ${att.name}")
          val paramNames:Seq[String] = e.attributes.map(att => s"${att.name}")

          Java(s"""${combinedOps}Exp ${e.name.capitalize}(${params.mkString(",")}) { return new $combinedOps${e.name.capitalize}(${paramNames.mkString(",")}); }""").methodDeclarations()
        })

        val getters: Seq[MethodDeclaration] =
          exp.attributes.flatMap(att => {
            // anything that is an EXPR can be replaced
            val cast = att.tpe match {
              case domain.baseTypeRep => if (!isBase) { s"(${baseInterface.toString})" } else { "" }
              case _ => ""
            }

            Java(s"""|public ${typeConverter(att.tpe, Some(baseInterface))} get${att.name.capitalize}() {
                     |    return $cast this.${att.name};
                     |}""".stripMargin).methodDeclarations()
          })

        val cons:Seq[Statement] = exp.attributes.flatMap(att => Java(s"  this.${att.name} = ${att.name};").statements())

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
            println(s"Skipping ${exp.name} for ${op.name}")
            Seq.empty
          } else {
            val md: MethodDeclaration = methodGenerator(exp)(op)

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
            val past: String = model.last.lastModelWithOperation().ops.sortWith(_.name < _.name).map(op => op.name.capitalize).mkString("")
            s"extends $past$name" // go backwards?
          }

        // include defined subtypes
        val definedSubTypes: Seq[BodyDeclaration[_]] = if (model.flatten().ops.exists {
          case bm: domain.BinaryMethodTreeBase => true
          case _ => false
        }) {
          definedDataSubTypes(s"${domain.AsTree.name.capitalize}${domain.baseTypeRep.name}", Seq(exp))
        } else {
          Seq.empty
        }

        Java(s"""
                |package interpreter;
                |public class $combinedOps$name $extension implements ${baseInterface.toString} {
                |  ${definedSubTypes.mkString("\n")}
                |  ${factoryMethods.mkString("\n")}
                |  ${constructor.toString}
                |
                |  ${getters.mkString("\n")}
                |  ${atts.mkString("\n")}
                |  ${operations.mkString("\n")}
                |}""".stripMargin).compilationUnit()
      })
  }
}
