package org.combinators.ep.approach.oo    /*DI:LI:AD*/

import org.combinators.ep.domain.{GenericModel, Model}
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command._
import org.combinators.ep.generator._
import org.combinators.ep.generator.communication._
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.{forEach, _}
import org.combinators.ep.generator.paradigm._
import org.combinators.ep.generator.paradigm.control.Imperative

sealed trait Interpreter extends OOApproachImplementationProvider with BaseDataTypeAsInterface with SharedOO with OperationInterfaceChain with FieldDefinition with FactoryConcepts {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]
  import ooParadigm._
  import paradigm._
  import syntax._

  val factoryName:String = "Factory"


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
      // goal is to find the class, i.e., "Sub", and have that become "ep.m4.Sub"
      rt <- findClass(names.mangle(names.conceptNameOf(tpeCase)))
      _ <- resolveAndAddImport(rt)

      res <- instantiateObject(rt, args)
    } yield res
  }

  /** Concatenate operations in model to derive unique name for classes. */
  def opsName(ops:Seq[Operation]):String = ops.sortWith(_.name < _.name).map(op => names.conceptNameOf(op)).mkString("")

  /**
   * Standard factory name for a dataTypeCase ignores the tpeCase and only focuses on the model
   *
   * {{{
   *   EvalExp
   * }}}
   *
   * Model is passed in should it become necessary to be overridden more specifically
   *
   * @param tpeCase    DataTypeCase for which a factory is desired.
   * @return
   */
  override def factoryNameDataTypeCase(model:Option[GenericModel] = None, tpeCase:DataTypeCase) : Name = {
    names.addSuffix(names.mangle(opsName(model.get.ops)), names.conceptNameOf(model.get.baseDataType))
  }

  /**
   * Standard factory name for a dataTypeCase ignores the tpeCase and only focuses on the model.
   * Needs model to be placed in right package
   *
   * {{{
   *   Add
   * }}}
   *
   * Model is passed in should it become necessary to be overridden more specifically
   *
   * @param tpeCase    DataTypeCase for which a factory is desired.
   * @return
   */
  override def factoryInstanceDataTypeCase(model:Option[GenericModel] = None, tpeCase:DataTypeCase) : Seq[Name] = {
    Seq(names.mangle(model.get.name), names.mangle(names.conceptNameOf(tpeCase)))
  }

  /** Find Model with operations and return that one's name as concatenations of operations. */
  def baseInterfaceName(m:Model): String = {
    if (m.lastModelWithOperation.isEmpty) {
      ""
    } else {
      m.lastModelWithOperation.head.ops.sortWith(_.name < _.name).map(op => names.conceptNameOf(op)).mkString("") + m.baseDataType.name
    }
  }

  /** Generate class for each DataTypeCase and Operation. Be sure to keep extension chain going when there are prior classes available.
   *
   * package ep.m2;
   *
   * public class Sub extends ep.m1.Sub implements Exp {
   *    ...
   * }
   *
   * @param model
   * @param ops
   * @param tpeCase
   * @param domainSpecific    Needed whenever an implementation is to be generated
   * @return
   */
  def makeClassForCase(model: Model, ops: Seq[Operation], tpeCase: DataTypeCase, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ClassContext, Unit] = {
    import classCapabilities._
    import ooParadigm.projectCapabilities._

    def addParentClass(): Generator[ClassContext, Unit] = if (!shouldAddParent()) {
      Command.skip[ClassContext]
    } else {
      // go to last model which had defined operation *OR* last model in which this tpeCase is defined.
      val modelDefiningType = model.findTypeCase(tpeCase)
      val lastDefiningOp = model.last.get.lastModelWithOperation    // go to last model which had defined operation (since it would have an Exp and thus a tpeCase)

      // could have better solution to deal with optionals.
      val chosenModel = modelDefiningType.getOrElse(lastDefiningOp.head).later(lastDefiningOp.head)

      val priorClass = names.mangle(names.conceptNameOf(tpeCase))   // WANT to get prior one in earlier package.
      for {

        priorType <- findClass(names.mangle(chosenModel.name), priorClass)
        _ <- resolveAndAddImport(priorType)
        _ <- addParent(priorType)
      } yield ()
    }

    // add a parent IF type defined earlier
    def shouldAddParent(): Boolean = {
      if (model.last.isDefined) {
        model.last.get.findTypeCase(tpeCase).isDefined
      } else {
        false
      }
    }

    for {
       pt <- toTargetLanguageType(TypeRep.DataType(model.baseDataType))
       _ <- resolveAndAddImport(pt)
       _ <- addImplemented(pt)

       _ <- addParentClass()
       _ <- forEach (tpeCase.attributes) { att => makeField(att) }

       _ <- forEach (model.flatten.typeCases) {
             tpe => {
               for {
                 priorType <- findClass(names.mangle(model.lastModelWithOperation.head.later(model.findTypeCase(tpe).get).name), names.mangle(names.conceptNameOf(tpe)))
                 _ <- registerLocally(DataType(tpe.name), priorType)
               } yield ()
             }
           }

       _ <- forEach (tpeCase.attributes) { att => makeCastableGetter(att) }
       _ <- addConstructor(makeConstructor(tpeCase, true, shouldAddParent()))

       _ <- forEach (ops) { op => addMethod(names.mangle(names.instanceNameOf(op)), makeImplementation(model.baseDataType, tpeCase, op, domainSpecific))
       }
    } yield ()
  }

  /** Interpreter has to go back to the former Model which had defined an operation */
  override def modelDeclaringInterfaceParent(model:Model) : Model = {
    model.last.getOrElse(model).lastModelWithOperation.headOption.getOrElse(model)     // instead of     getOrElse(model)
  }

  /** Place operation classes in appropriate package.  */
  def operationNames(domain: Model, tpe:DataTypeCase): Seq[Name] = {
    Seq(names.mangle(domain.name), names.mangle(names.conceptNameOf(tpe)))
  }

  def generateForOp(model:Model, ops:Seq[Operation], defining:Model, allTypes:Seq[DataTypeCase], domainSpecific: EvolutionImplementationProvider[this.type]) : Generator[ProjectContext, Unit] = {
    val combinedOps:String = ops.sortWith(_.name < _.name).map(op => names.conceptNameOf(op)).mkString("")
    import ooParadigm.projectCapabilities._
    import paradigm.projectContextCapabilities.debug

    for {
      _ <- forEach (allTypes) { tpeCase => {
         for {
           _ <- addClassToProject(makeClassForCase(model, ops, tpeCase, domainSpecific), operationNames(defining, tpeCase) : _ *)
        } yield ()
        }
      }
    } yield ()
  }

  /**
   * Factories must also support all future data types. For example, the EvalExpFactory
   * naturally supports Lit and Add, but it must support all future data types as well.
   *
   * These generated code files are part of the Construction code and are meant to ease
   * the ability of clients to construct instances of the EP
   *
   * {{{
   *   public class EvalIdzExpFactory {
   *
   *     public static EvalIdzExp Neg(EvalIdzExp inner) {
   *         return new EvalIdzNeg(inner);
   *     }
   *
   *     ...
   * }
   * }}}
   *
   * @param model
   * @return
   */
  def generateFactory(model: Model): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    def factoryClass(model: Model): Generator[ClassContext, Unit] = {
      import ooParadigm.classCapabilities._
      for {
        opClass <- toTargetLanguageType(TypeRep.DataType(model.baseDataType))   // should check!
        _ <- forEach(model.pastDataTypes) {
          tpe =>   // returnType and paramType can be the same for Interpreter
            addMethod(names.mangle(names.conceptNameOf(tpe)), createStaticFactoryDataTypeCase(model, tpe, opClass, opClass))
        }
      } yield ()
    }

    addClassToProject(factoryClass(model), Seq(names.mangle(model.name), names.mangle(factoryName)) : _ * )
  }

  /** For Interpreter, the covariant type needs to be selected whenever a BaseType in the domain is expressed. */
  def domainTypeLookup[Ctxt](covariantType: Name*)(implicit canFindClass: Understands[Ctxt, FindClass[Name, Type]]): Generator[Ctxt, Type] = {
    FindClass(covariantType).interpret(canFindClass)
  }

  /** Enables mapping each DataType to the designated ep.?.DT class. */
  def registerLocally(tpe:DataType, paramType:Type) : Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._

    val dtpeRep = TypeRep.DataType(tpe)
    println ("register " + tpe.name + " with " + paramType)

    for {
      _ <- addTypeLookupForMethods(dtpeRep, Command.lift(paramType))
      _ <- addTypeLookupForClasses(dtpeRep, Command.lift(paramType))
      _ <- addTypeLookupForConstructors(dtpeRep, Command.lift(paramType))

    } yield ()
  }

  /** What model is delivered has operations which is essential for the mapping. */
  override def registerTypeMapping(model: GenericModel): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    import ooParadigm.classCapabilities.canFindClassInClass
    import ooParadigm.constructorCapabilities.canFindClassInConstructor
    import ooParadigm.methodBodyCapabilities.canFindClassInMethod
    import paradigm.projectContextCapabilities._

    // must always be an operation in FIRST evolution  MISSING FULLY QUALIFIED NAMES HERE HACK
    // TODO: FIX HERE
    val baseInterface = baseInterfaceNames(model.lastModelWithOperation.head)
    val dtpeRep = TypeRep.DataType(model.baseDataType)
    for {
      _ <- addTypeLookupForMethods(dtpeRep, domainTypeLookup(baseInterface : _*))
      _ <- addTypeLookupForClasses(dtpeRep, domainTypeLookup(baseInterface : _*))
      _ <- addTypeLookupForConstructors(dtpeRep, domainTypeLookup(baseInterface : _*))
    } yield ()
  }

  def implement(gdomain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {

    val domain = gdomain match {
      case _:Model => gdomain.asInstanceOf[Model]
      case _ => gdomain.linearize
    }

    /**
     * For each operation must generate a sequence of classes, one per subtype.
     * Must make sure we include ALL subtypes, not just ones from the past.
     */
    for {
      _ <- registerTypeMapping(domain)
      _ <- domainSpecific.initialize(this)

      _ <- makeBase(domain.baseDataType, Seq.empty)

     // required to do this first so interface/types are found
      _ <- forEach (domain.inChronologicalOrder) { model => {
        if (model.ops.nonEmpty) {
          for {
            _ <- registerTypeMapping(model) // this must be first SO Exp is properly bound within interfaces
            _ <- addIntermediateInterfaceToProject(model, domainSpecific, None)

            _ <- generateForOp(model, model.ops, model, model.pastDataTypes,domainSpecific)

            _ <- if (model.typeCases.nonEmpty) {
              for {
                _ <- forEach(model.toSeq.filter(mi => mi.ops.nonEmpty)) { mi =>
                  generateForOp(mi, mi.ops, model, model.typeCases, domainSpecific)
                }
              } yield ()
            } else {
              Command.skip[ProjectContext]
            }

            _ <- generateFactory(model)
          } yield ()
        } else {
          // no operations. Must spit out new types and pull together all operations from the past.
          generateForOp(model.lastModelWithOperation.head, model.flatten.ops, model, model.typeCases, domainSpecific)
        }
      }
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
     imp: Imperative.WithBase[base.MethodBodyContext, base.type],
     oo: ObjectOriented.WithBase[base.type],
     parametricPolymorphism: ParametricPolymorphism.WithBase[base.type])
    (generics: Generics.WithBase[base.type, oo.type, parametricPolymorphism.type]): Interpreter.WithParadigm[base.type] =
      new Interpreter {
        val paradigm: base.type = base
        val names: NameProvider[paradigm.syntax.Name] = nameProvider
        val impParadigm: imp.type = imp
        val ooParadigm: oo.type = oo
        val polymorphics: parametricPolymorphism.type = parametricPolymorphism
        val genericsParadigm: generics.type = generics
      }
  }
