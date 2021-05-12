package org.combinators.ep.approach.oo    /*DI:LI:AD*/

import org.combinators.ep.domain.{GenericModel, Model}
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command._
import org.combinators.ep.generator._
import org.combinators.ep.generator.communication._
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.{forEach, _}
import org.combinators.ep.generator.paradigm._
import org.combinators.ep.generator.paradigm.control.Imperative

sealed trait Interpreter extends OOApproachImplementationProvider with BaseDataTypeAsInterface with SharedOO with OperationInterfaceChain with FieldDefinition {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val polymorphics: ParametricPolymorphism.WithBase[paradigm.type]
  val genericsParadigm: Generics.WithBase[paradigm.type, ooParadigm.type, polymorphics.type]
  import ooParadigm._
  import paradigm._
  import syntax._

  // Critical aspect of CoCo is that the Extended Intermediate Interface (i.e., ep.m3.Exp) is only created when
  // needed, specifically: (a) a new operation is being defined, and this interface will host the default
  // implementation; or (b) a branch is being merged from branches in which new Exp had been defined
  // useful when determining merging
  def ancestorsDefiningNewTypeInterfaces(domain: GenericModel): Set[GenericModel] = {
    val ancestorsWithNewTypeInterfaces = domain.former.map(ancestor => latestModelDefiningNewTypeInterface(ancestor))
    ancestorsWithNewTypeInterfaces.distinct.filterNot { ancestor =>
      // get rid of everything that has an antecedent
      ancestorsWithNewTypeInterfaces.exists(otherAncestor => ancestor.before(otherAncestor))
    }.toSet
  }

  def latestModelDefiningNewTypeInterface(domain: GenericModel): GenericModel = {
    if (domain.isDomainBase || domain.ops.nonEmpty) {
      domain
    } else {
      // is there a single type that can represent the "least upper bound" of all prior branches.
      val ancestorsWithTypeInterfaces = ancestorsDefiningNewTypeInterfaces(domain)
      if (ancestorsWithTypeInterfaces.size == 1 && !ancestorsWithTypeInterfaces.head.isDomainBase) { // take care to avoid falling below "floor"
        ancestorsWithTypeInterfaces.head
      } else {
        domain // we have to do merge
      }
    }
  }

  def mostSpecificBaseInterfaceType[Context](domain: GenericModel)(implicit
      canFindClass: Understands[Context, FindClass[paradigm.syntax.Name, paradigm.syntax.Type]],
      canResolveImport: Understands[Context, ResolveImport[paradigm.syntax.Import, paradigm.syntax.Type]],
      canAddImport: Understands[Context, AddImport[paradigm.syntax.Import]],
      canApplyType: Understands[Context, Apply[paradigm.syntax.Type, paradigm.syntax.Type, paradigm.syntax.Type]],
  ): Generator[Context, paradigm.syntax.Type] = {

    val resultModelStage = latestModelDefiningNewTypeInterface(domain)

    for {
      baseInterfaceType <-
        if (resultModelStage.isDomainBase) {
          FindClass[paradigm.syntax.Name, paradigm.syntax.Type](Seq(names.mangle(names.conceptNameOf(domain.baseDataType)))).interpret(canFindClass)
        } else {
          FindClass[paradigm.syntax.Name, paradigm.syntax.Type](Seq(names.mangle(names.instanceNameOf(resultModelStage)), names.mangle(names.conceptNameOf(domain.baseDataType)))).interpret(canFindClass)
        }
      _ <- resolveAndAddImport(baseInterfaceType)

    } yield baseInterfaceType
  }

  def appropriateCast(model: Model) : String = {
    // go to last model which had defined operation *OR* last model in which this tpeCase is defined.
    if (model.last.isEmpty) {
      return opsName(model.ops)
    }
    val modelDefiningOp = model.lastModelWithOperation

    val lastDefiningOp = model.last.get.lastModelWithOperation    // go to last model which had defined operation (since it would have an Exp and thus a tpeCase)

    // could have better solution to deal with multiples.
    val chosenModel = modelDefiningOp.head.later(lastDefiningOp.head)

    if (chosenModel.ops.nonEmpty) {
      opsName(chosenModel.ops)
    } else {
      opsName(chosenModel.lastModelWithOperation.flatMap(m => m.ops))
    }
  }


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
      // Using 'toTargetLanguage' brings in the type from where it had been registered using registerTypes
      rt <- toTargetLanguageType(TypeRep.DataType(DataType(tpeCase.name)))       //findClass(names.mangle(names.conceptNameOf(tpeCase)))
      _ <- resolveAndAddImport(rt)

      res <- instantiateObject(rt, args)
    } yield res
  }

  /** Concatenate operations in model to derive unique name for classes. */
  def opsName(ops:Seq[Operation]):String = ops.sortWith(_.name < _.name).map(op => names.conceptNameOf(op)).mkString("")

  /** Find Model with operations and return that one's name as concatenations of operations. */
  def baseInterfaceName(m:Model): String = {
    if (m.lastModelWithOperation.isEmpty) {
      ""
    } else {
      m.lastModelWithOperation.head.ops.sortWith(_.name < _.name).map(op => names.conceptNameOf(op)).mkString("") + m.baseDataType.name
    }
  }

  def makeInterpreterImplementation(domain: GenericModel,
                          tpe: DataType,
                          tpeCase: DataTypeCase,
                          op: Operation,
                          domainSpecific: EvolutionImplementationProvider[this.type]
                        ): Generator[MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import polymorphics.methodBodyCapabilities._
    for {
      _ <- makeSignature(op)
      thisRef <- selfReference()
      attAccessors: Seq[Expression] <- forEach (tpeCase.attributes) { att =>
        for {
          att_member <- getMember(thisRef, names.mangle(names.instanceNameOf(att)))
          tpeCase <- mostSpecificBaseInterfaceType(domain)
          casted <- if (att.tpe.isModelBase(domain)) {    // only cast if Exp
            castObject(tpeCase, att_member)
          } else {
            Command.lift[MethodBodyContext, Expression](att_member)
          }
        } yield casted
      }

      atts = tpeCase.attributes.zip(attAccessors).toMap

      allArgs <- getArguments()
      castedArgs <- forEach(op.parameters.zip(allArgs)) { case (param,arg) => {
        for {
          tpeCase <- mostSpecificBaseInterfaceType(domain)
          casted <- if (param.tpe.isModelBase(domain)) {    // only cast if Exp
            castObject(tpeCase, arg._3)
          } else {
            Command.lift[MethodBodyContext, Expression](arg._3)
          }
        } yield (param,casted)
      }}

      castedArgsMap = castedArgs.toMap

      result <-
        domainSpecific.logic(this)(
          ReceivedRequest(
            tpe,
            tpeCase,
            thisRef,
            atts,
            Request(op, castedArgsMap)
          )
        )
    } yield result
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

      val defOps = if (chosenModel.ops.nonEmpty) {
        opsName(chosenModel.ops)
      } else {
        opsName(chosenModel.lastModelWithOperation.flatMap(m => m.ops))
      }

      //   but this won't always work SINCE this might not have the operations. Need to find the prior one that
      //   was constructed (defOps was opsName(chosenModel.ops))
      val priorClass = defOps + names.conceptNameOf(tpeCase)
      for {

        priorType <- findClass(names.mangle(chosenModel.name), names.mangle(priorClass))
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
       _ <- if (!shouldAddParent()) {
         for {
           _ <- forEach(tpeCase.attributes) { att => makeField(att) }
         } yield ()
       } else {
         Command.skip[ClassContext]
       }

       _ <- forEach (model.flatten.typeCases) {
             tpe => {
               for {
                 priorType <- findClass(names.mangle(model.lastModelWithOperation.head.later(model.findTypeCase(tpe).get).name), names.mangle(names.conceptNameOf(tpe)))
                ///// _ <- registerLocally(DataType(tpe.name), priorType)
               } yield ()
             }
           }

       // if super is being used, then you need to cast to Exp
       _ <- addConstructor(makeConstructor(tpeCase, !shouldAddParent(), useSuper = shouldAddParent()))

       _ <- forEach (ops) { op => addMethod(names.mangle(names.instanceNameOf(op)), makeInterpreterImplementation(model, model.baseDataType, tpeCase, op, domainSpecific))
       }
    } yield ()
  }

  /** Interpreter has to go back to the former Model which had defined an operation */
  override def modelDeclaringInterfaceParent(model:Model) : Model = {
    model.last.getOrElse(model).lastModelWithOperation.headOption.getOrElse(model)     // instead of     getOrElse(model)
  }

  /** Place operation classes in appropriate package. Include ops names as well */
  def operationNames(domain: Model, tpe:DataTypeCase, ops:Seq[Operation]): Seq[Name] = {
    val concat = names.mangle(ops.sortWith(_.name < _.name).map(op => names.conceptNameOf(op)).mkString(""))
    Seq(names.mangle(domain.name), names.mangle(concat + names.conceptNameOf(tpe)))
  }

  def generateForOp(model:Model, ops:Seq[Operation], defining:Model, allTypes:Seq[DataTypeCase], domainSpecific: EvolutionImplementationProvider[this.type]) : Generator[ProjectContext, Unit] = {
    val combinedOps:String = ops.sortWith(_.name < _.name).map(op => names.conceptNameOf(op)).mkString("")
    import ooParadigm.projectCapabilities._
    import paradigm.projectContextCapabilities.debug

    for {
      _ <- forEach (allTypes) { tpeCase => {
         for {
           _ <- addClassToProject(makeClassForCase(model, ops, tpeCase, domainSpecific), operationNames(defining, tpeCase, ops) : _ *)
        } yield ()
        }
      }
    } yield ()
  }

  /** For Interpreter, the covariant type needs to be selected whenever a BaseType in the domain is expressed. */
  def domainTypeLookup[Ctxt](covariantType: Name*)(implicit canFindClass: Understands[Ctxt, FindClass[Name, Type]]): Generator[Ctxt, Type] = {
    FindClass(covariantType).interpret(canFindClass)
  }

  def paramType[Context](domain:GenericModel, tpe:DataTypeCase)(implicit
         canFindClass: Understands[Context, FindClass[paradigm.syntax.Name, paradigm.syntax.Type]],
         canResolveImport: Understands[Context, ResolveImport[paradigm.syntax.Import, paradigm.syntax.Type]],
         canAddImport: Understands[Context, AddImport[paradigm.syntax.Import]]
        ) : Generator[Context, Type] = {
    println("registerParamType " + domain.name + " with " + tpe.name)
    for {
      cname <- FindClass[paradigm.syntax.Name, paradigm.syntax.Type](Seq(names.mangle(domain.name), names.mangle(names.conceptNameOf(tpe)))).interpret(canFindClass)
      _ <- resolveAndAddImport(cname)
    } yield cname
  }
  /** Enables mapping each DataType to the designated ep.?.DT class. */
  def registerTypeCases(domain:GenericModel) : Generator[ProjectContext, Unit] = {
    import ooParadigm.classCapabilities.{addTypeLookupForMethods => _, addTypeLookupForClasses => _, addTypeLookupForConstructors => _,_}
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import ooParadigm.constructorCapabilities._
    import ooParadigm.projectCapabilities._
    import paradigm.projectContextCapabilities._
    // domain.typeCases.
    for {
      _ <- forEach(domain.flatten.typeCases) { tpe =>
        for {
          _ <- addTypeLookupForMethods(TypeRep.DataType(DataType(tpe.name)), paramType[MethodBodyContext](domain, tpe))
          _ <- addTypeLookupForClasses(TypeRep.DataType(DataType(tpe.name)), paramType[ClassContext](domain, tpe))
          _ <- addTypeLookupForConstructors(TypeRep.DataType(DataType(tpe.name)), paramType[ConstructorContext](domain, tpe))
        } yield ()
      }
    } yield()
  }

  /** Enables mapping each DataType to the designated ep.?.DT class. */
  def registerLocally(tpe:DataType, paramType:Type) : Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._

    val dtpeRep = TypeRep.DataType(tpe)
    println ("register " + tpe.name + " with " + paramType)

    for {
//      _ <- addTypeLookupForMethods(dtpeRep, Command.lift(paramType))
//      _ <- addTypeLookupForClasses(dtpeRep, Command.lift(paramType))
//      _ <- addTypeLookupForConstructors(dtpeRep, Command.lift(paramType))
        _ <- debug("something")
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
            _ <- registerTypeCases(model)   // handle DataType classes as well for interpreter
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
          } yield ()
        } else {
          // no operations. Must spit out new types and pull together all operations from the past.
          for {
            _ <- registerTypeCases(model)   // handle DataType classes as well for interpreter
            _ <- generateForOp(model.lastModelWithOperation.head, model.flatten.ops, model, model.typeCases, domainSpecific)
          } yield ()
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
