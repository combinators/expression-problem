package org.combinators.ep.approach.oo

import org.combinators.ep.domain.Model
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command._
import org.combinators.ep.generator._
import org.combinators.ep.generator.communication._
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax._
import org.combinators.ep.generator.paradigm._


// copied from visitor to start
sealed trait Interpreter extends OOApproachImplementationProvider with OperationInterfaceChain with FieldDefinition {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]
  import ooParadigm._
  import paradigm._
  import syntax._

  def dispatch(message: SendRequest[Expression]): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    for {
      method <- getMember(message.to, names.mangle(names.instanceNameOf(message.request.op)))
      result <- apply(method, message.request.op.parameters.map(message.request.arguments))
    } yield result

  }

  def instantiate(baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._

    for {
      rt <- findClass(names.mangle(names.conceptNameOf(tpeCase)))

      _ <- resolveAndAddImport(rt)
      res <- instantiateObject(rt, args)
    } yield res
  }


  def makeBaseExtensions(model:Model): Generator[ProjectContext, Unit] = {
    val allTypes = model.pastDataTypes
    val isBase: Boolean = model.base.equals(model)
    generateForOp(model, model.ops, allTypes, isBase)
  }

  def opsName(ops:Seq[Operation]):String = ops.sortWith(_.name < _.name).map(op => names.conceptNameOf(op)).mkString("")

  /** Find Model with operations and return that one's name as concatenations of operations. */
  def baseInterfaceName(m:Model): String = {
    if (m.lastModelWithOperation.isEmpty) {
      ""
    } else {
      m.lastModelWithOperation.get.ops.sortWith(_.name < _.name).map(op => names.conceptNameOf(op)).mkString("") + "Exp"
    }
  }
  /**
   * This is what I generated before, one for each subtype and the operations defined within it.
   *
   * package interpreter;
   * ${factoryImports.mkString("\n")}
   * ${liftedOps.mkString("\n")}
   * public class $combinedOps$name $extension implements ${baseInterface.toString} {
   *   ${constructor.toString}
   *
   *   ${getters.mkString("\n")}
   *   ${atts.mkString("\n")}
   *   ${operations.mkString("\n")}
   *   ${conversionMethod.mkString("\n")}
   * }""".stripMargin).compilationUnit()
   *
   * @param model
   * @param ops
   * @return
   */
  def makeClassForCase(model: Model, ops: Seq[Operation], tpeCase: DataTypeCase): Generator[ClassContext, Unit] = {
    import classCapabilities._
    import ooParadigm.projectCapabilities._
    for {
       pt <- toTargetLanguageType(TypeRep.DataType(model.baseDataType))  // TODO: How do I get DataType from DataTypeCase?
      _ <- resolveAndAddImport(pt)
      _ <- addParent(pt)
      _ <- forEach(tpeCase.attributes) { att => makeField(att) }
    } yield ()
  }

  def generateForOp(model:Model, ops:Seq[Operation], allTypes:Seq[DataTypeCase], isBase:Boolean) : Generator[ProjectContext, Unit] = {
    val combinedOps:String = ops.sortWith(_.name < _.name).map(op => names.conceptNameOf(op)).mkString("")
    import ooParadigm.projectCapabilities._

    for {
      _ <- forEach (allTypes) { tpeCase =>
        addClassToProject(names.mangle(combinedOps), makeClassForCase(model, ops, tpeCase))
      }
    } yield ()

  }

  def implement(domain: Model, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {

    /**
     * For each operation must generate a sequence of classes, one per subtype.
     * Must make sure we include ALL subtypes, not just ones from the past.
     */
    for {
      _ <- registerTypeMapping(domain)
      _ <- domainSpecific.initialize(this)
      _ <- makeBase(domain.baseDataType, Seq.empty)     // Marker interface -- ignores operations
      _ <- forEach (domain.inChronologicalOrder.filter(m => m.ops.nonEmpty)) { m => generateForOp(m, m.ops, m.pastDataTypes, m.isBase) }

      _ <- forEach (domain.inChronologicalOrder.filter(m => m.ops.nonEmpty)) { model =>
        // for all PAST dataTypes that are already defined
         makeIntermediateInterface(model, domainSpecific)
       }

    } yield ()
  }
}

  object Interpreter {
    type WithParadigm[P <: AnyParadigm] = Interpreter { val paradigm: P }
    type WithSyntax[S <: AbstractSyntax] = WithParadigm[AnyParadigm.WithSyntax[S]]

    def apply[S <: AbstractSyntax, P <: AnyParadigm.WithSyntax[S]]
    (base: P)
    (nameProvider: NameProvider[base.syntax.Name],
     oo: ObjectOriented.WithBase[base.type],
     parametricPolymorphism: ParametricPolymorphism.WithBase[base.type])
    (generics: Generics.WithBase[base.type, oo.type, parametricPolymorphism.type]): Interpreter.WithParadigm[base.type] =
      new Interpreter {
        val paradigm: base.type = base
        val names: NameProvider[paradigm.syntax.Name] = nameProvider
        val ooParadigm: oo.type = oo
        val polymorphics: parametricPolymorphism.type = parametricPolymorphism
        val genericsParadigm: generics.type = generics
      }
  }
