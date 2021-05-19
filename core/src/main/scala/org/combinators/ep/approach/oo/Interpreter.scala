package org.combinators.ep.approach.oo    /*DI:LI:AD*/

import org.combinators.ep.domain.{GenericModel, Model}
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command._
import org.combinators.ep.generator._
import org.combinators.ep.generator.communication._
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax.{forEach, _}
import org.combinators.ep.generator.paradigm._
import org.combinators.ep.generator.paradigm.control.Imperative

sealed trait Interpreter extends OOApproachImplementationProvider with BaseDataTypeAsInterface with SharedOO with FieldDefinition {
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

  /// ------------------------------------------------------ Operator As Chain Moved Here ---------------

  // TODO: should be revised to pass in the base, and not presume it
  def baseInterfaceNames(domain: GenericModel): Seq[Name] = {
    if (domain.isDomainBase) {
      // ignore MathDomain, for example, and just grab name...
      Seq(names.mangle(domain.baseDataType.name))
    } else {
      Seq(names.mangle(domain.name), names.mangle(domain.baseDataType.name))
    }
  }

//  // extends Exp [first one] or ExpEval [previous one]
//  // Works for both Exp* interface declarations as well as DataTypeOp declarations
//  def getParentInterface(domain: GenericModel): Generator[ClassContext, Type] = {
//    import classCapabilities._
//
//    if (domain.isEmpty || domain.lastModelWithOperation.isEmpty) {
//      findClass(names.mangle(domain.baseDataType.name))
//    } else {
//      findClass(baseInterfaceNames(domain) : _*)
//    }
//  }

  /**
   * Make sure to include past producer methods as well...
   */
  def makeInterface(domain:GenericModel): Generator[ClassContext, Unit] = {
    // create class which is an interface containing abstract methods
    import classCapabilities._
    import genericsParadigm.classCapabilities._

    for {
      _ <- setInterface()

      // former merge points need to be included, so  past.lastModelWithOperation) is changed to below
      _ <- forEach(domain.former.map(past => latestModelDefiningInterface(past))) { m => for {
           /** Interpreter has to go back to the former Model which had defined an operation */
           //parent <- getParentInterface(m)
           parent <- if (m.isDomainBase) {
             findClass(names.mangle(names.conceptNameOf(m.baseDataType)))
           } else {
             findClass(names.mangle(names.instanceNameOf(m)), names.mangle(names.conceptNameOf(m.baseDataType)))
           }
           _ <- resolveAndAddImport(parent)
           _ <- addParent(parent)
         } yield ()
        }

      // grab current operations AND all producers. Do together to eliminate duplicates.
      _ <- forEach ((domain.ops ++ domain.pastOperations.filter(op => op.isProducer(domain))).distinct) { op => addAbstractMethod(names.mangle(names.instanceNameOf(op)), makeInterpreterSignature(domain, op)) }
    } yield ()
  }

  /**
   * Create intermediate interfaces that form a chain of operation definitions.
   * Now this is created EVEN when an evolution doesn't create an operation.
   *
   * Make sure producer methods are subsequently copied...
   * @param domainSpecific
   * @return
   */
  def addIntermediateInterfaceToProject(domain:GenericModel): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    addClassToProject(makeInterface(domain), baseInterfaceNames(domain) : _*)  }

  /// -------------------------------------^^^^^^^^^^^^^^^^^^^^^^^^^^^^^-----------------------------------------------

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
//      rt <- findClass(names.mangle(tpeCase.name))
      _ <- resolveAndAddImport(rt)   // SEEMS to have no impact for data type classess...

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

  /** Create signature using contravariant baseDomain type from past Exps.
   *
   * {{{
   *   public Double OPERATION(PARAM...)
   * }}}
   * @param op
   * @return
   */
  def makeInterpreterSignature(domain:GenericModel, op: Operation): Generator[MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      rt <- toTargetLanguageType(op.returnType)   // covariant overriding is still feasible
      _ <- resolveAndAddImport(rt)
      _ <- setReturnType(rt)
      params <- forEach (op.parameters) { param: Parameter =>
        for {// if a base type then must use original
          pt <- if (param.tpe.isModelBase(domain)) {
            findClass(names.mangle(names.instanceNameOf(domain.findOperation(op).get)), names.mangle(names.conceptNameOf(domain.baseDataType)))
          } else {
            toTargetLanguageType(param.tpe)
          }
          _ <- resolveAndAddImport(pt)
          pName <- freshName(names.mangle(param.name))
        } yield (pName, pt)
      }
      _ <- setParameters(params)
    } yield ()
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
      _ <- makeInterpreterSignature(domain, op)
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

  def latestModelDefiningInterface(domain: GenericModel): GenericModel = {
    if (domain.isDomainBase || domain.ops.nonEmpty || domain.former.length > 1) {   // handle merge case as well
      domain
    } else {
      // find where tpe was defined and also where last operation was defined and choose later of the two
      // will only have one, since merge case handled above.
      // could be one of our ancestors is a merge point
      latestModelDefiningInterface(domain.former.head)
      //domain.lastModelWithOperation.head
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
   * @return
   */
  def makeClassForCase(model: GenericModel, ops: Seq[Operation], tpeCase: DataTypeCase,
           domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ClassContext, Unit] = {
    import classCapabilities._

    def primaryParent(): GenericModel = {
      val modelDefiningType = model.findTypeCase(tpeCase)
      val pastModel = model.former.map(m => latestModelDefiningInterface(m)).head
      modelDefiningType.getOrElse(pastModel).later(pastModel)
    }

    // if an ancestor branch doesn't define parent then add all operations

    // Even though there *may* be multiple formers (due to a merge) we can't actually add multiple
    // parents. If there are multiple, then must find which branch defines the tpeCase and choose
    // that one -- if both do, then it doesn't matter which one you choose, so to be consistent
    // always choose the first one.
    def addPrimaryParent(): Generator[ClassContext, Unit] = {
      if (!shouldAddParent()) {
       Command.skip[ClassContext]
      } else {
        // go to last model which had defined operation *OR* last model in which this tpeCase is defined.
        val chosenModel = primaryParent()
        for {
          priorType <- findClass(names.mangle(chosenModel.name), names.mangle(names.conceptNameOf(tpeCase)))
            _ <- resolveAndAddImport(priorType)
            _ <- addParent(priorType)
          } yield ()
        }
      }

    // add a parent IF type defined earlier, in ANY of its formers..
    def shouldAddParent(): Boolean = {
      model.former.exists(m => m.findTypeCase(tpeCase).isDefined)
    }

    for {  // find latest model with operation, since that will be the proper one to use
       //pt <- toTargetLanguageType(TypeRep.DataType(model.baseDataType))
       pt <- findClass(names.mangle(names.instanceNameOf(latestModelDefiningInterface(model))), names.mangle(names.conceptNameOf(model.baseDataType)))
       _ <- resolveAndAddImport(pt)
       _ <- addImplemented(pt)

       _ <- addPrimaryParent()
       _ <- if (!shouldAddParent()) {
         for {
           _ <- forEach(tpeCase.attributes) { att => makeField(att) }
         } yield ()
       } else {
         Command.skip[ClassContext]
       }

       // if super is being used, then you need to cast to Exp
       _ <- addConstructor(makeConstructor(tpeCase, !shouldAddParent(), useSuper = shouldAddParent()))

       _ <- forEach (ops) { op => addMethod(names.mangle(names.instanceNameOf(op)), makeInterpreterImplementation(model, model.baseDataType, tpeCase, op, domainSpecific))
       }
    } yield ()
  }

  /** Place operation classes in appropriate package. Include ops names as well */
  def qualifiedDataTypeCase(domain: GenericModel, tpe:DataTypeCase): Seq[Name] = {
    Seq(names.mangle(domain.name), names.mangle(names.conceptNameOf(tpe)))
  }

  def generateForOpForType(ops:Seq[Operation], defining:GenericModel, tpeCase:DataTypeCase, domainSpecific: EvolutionImplementationProvider[this.type]) : Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    for {
      _ <- addClassToProject(makeClassForCase(defining, ops, tpeCase, domainSpecific), qualifiedDataTypeCase(defining, tpeCase) : _ *)
    } yield ()
  }

  /** Class is generated into 'defining' namespace package.  */
  def generateForOp(ops:Seq[Operation], defining:GenericModel, allTypes:Seq[DataTypeCase], domainSpecific: EvolutionImplementationProvider[this.type]) : Generator[ProjectContext, Unit] = {
    for {
        _ <- forEach (allTypes) { tpeCase => generateForOpForType(ops, defining, tpeCase, domainSpecific)
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

  def latestModelDefiningNewTypeInterface(domain: GenericModel, tpe:DataTypeCase): GenericModel = {
    if (domain.isDomainBase || domain.ops.nonEmpty || domain.former.length > 1) {   // handle merge case as well
      domain
    } else {
      // find where tpe was defined and also where last operation was defined and choose later of the two
      val tpeDefined = domain.findTypeCase(tpe).get
      val lastOps = latestModelDefiningInterface(domain)
      lastOps.later(tpeDefined)
     }
  }

  /** Enables mapping each DataType to the designated ep.?.DT class. */
  def registerTypeCases(domain:GenericModel) : Generator[ProjectContext, Unit] = {
    import ooParadigm.classCapabilities.{addTypeLookupForMethods => _, addTypeLookupForClasses => _, addTypeLookupForConstructors => _,_}
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import ooParadigm.constructorCapabilities._
    import ooParadigm.projectCapabilities._
    import paradigm.projectContextCapabilities._

    for {
      _ <- forEach(domain.flatten.typeCases) { tpe => {
          val tpeDomain = latestModelDefiningNewTypeInterface(domain, tpe)
          for {
            _ <- addTypeLookupForMethods(TypeRep.DataType(DataType(tpe.name)), paramType[MethodBodyContext](tpeDomain, tpe))
            _ <- addTypeLookupForClasses(TypeRep.DataType(DataType(tpe.name)), paramType[ClassContext](tpeDomain, tpe))
            _ <- addTypeLookupForConstructors(TypeRep.DataType(DataType(tpe.name)), paramType[ConstructorContext](tpeDomain, tpe))
          } yield ()
        }
      }
    } yield()
  }

  /** What model is delivered has operations which is essential for the mapping. */
  override def registerTypeMapping(model: GenericModel): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    import ooParadigm.classCapabilities.canFindClassInClass
    import ooParadigm.constructorCapabilities.canFindClassInConstructor
    import ooParadigm.methodBodyCapabilities.canFindClassInMethod
    import paradigm.projectContextCapabilities._

    // must always be an operation in FIRST evolution
    // If MERGED, then must derive new one
    val baseInterface = baseInterfaceNames(latestModelDefiningInterface(model))  // model.lastModelWithOperation.head)

    val dtpeRep = TypeRep.DataType(model.baseDataType)
    for {
      _ <- addTypeLookupForMethods(dtpeRep, domainTypeLookup(baseInterface : _*))
      _ <- addTypeLookupForClasses(dtpeRep, domainTypeLookup(baseInterface : _*))
      _ <- addTypeLookupForConstructors(dtpeRep, domainTypeLookup(baseInterface : _*))
    } yield ()
  }

  def primaryParent(model:GenericModel, tpeCase:DataTypeCase): GenericModel = {
    val modelDefiningType = model.findTypeCase(tpeCase)
    val pastModel = model.former.flatMap(m => m.lastModelWithOperation).head
    modelDefiningType.getOrElse(pastModel).later(pastModel)
  }

  // find those operations that are not defined by the primary parent
  def nonPrimaryParentOps(domain:GenericModel, tpe:DataTypeCase) : Seq[Operation] = {
    (domain.flatten.ops.toSet -- primaryParent(domain, tpe).flatten.ops).toSeq
  }

  // Have to process models in chronological order to get the Exp mappings properly done.
  // make sure that LATER merges do not generate.
  def implement(model: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[paradigm.ProjectContext, Unit] = {
    def implementInner(domain: GenericModel): Generator[paradigm.ProjectContext, Unit] = {

      val opsToHandle = (domain.ops ++ domain.flatten.ops.distinct.filter(op => op.isProducer(domain))).distinct
      if (domain.isDomainBase) {
        Command.skip[paradigm.ProjectContext]
      } else {
        for {
          _ <- registerTypeMapping(domain) // this must be first SO Exp is properly bound within interfaces
          _ <- registerTypeCases(domain)   // handle DataType classes as well for interpreter
          _ <- if (domain.ops.nonEmpty) {   // MERGE must be here as well...
            for {
             _ <- addIntermediateInterfaceToProject(domain)   // Exp for each evolution that needs one

              // needed for MERGE situation... (was domain.flatten.ops.distinct)
              _ <- generateForOp(opsToHandle, domain, domain.flatten.typeCases, domainSpecific)
            } yield ()
          } else {
           Command.skip[paradigm.ProjectContext]
          }   //
          // if extending a past operation chain, NO NEED for past ops
          // new data types need all operations
          _ <- generateForOp(domain.flatten.ops.distinct, domain, domain.typeCases, domainSpecific)

          // new operations (and producers from the past) need generation for types IF a new Exp is required.
          _ <- if (domain == latestModelDefiningInterface(domain)) {
            for {
               _ <- forEach(domain.former.flatMap(m => m.typeCases).distinct) { tpe => {
                 generateForOpForType((nonPrimaryParentOps(domain, tpe) ++ domain.ops ++ domain.pastOperations.filter(op => op.isProducer(domain))).distinct,
                   domain, tpe, domainSpecific)
               }}
            } yield ()
          } else {
            Command.skip[paradigm.ProjectContext]
          }
        } yield()
      }
    }

    /** have to choose appropriate branch for each data type, and then re-implement non-producer
     * methods that are not inherited.
     *  */
    def implementMerged(domain: GenericModel): Generator[paradigm.ProjectContext, Unit] = {
      val opsToHandle = domain.flatten.ops.distinct
       for {
          _ <- registerTypeCases(domain)   // handle DataType classes as well for interpreter

          _ <- registerTypeMapping(domain) // this must be first SO Exp is properly bound within interfaces
          _ <- addIntermediateInterfaceToProject(domain)   // Exp for each evolution that needs one

          // needed for MERGE situation... (was domain.flatten.ops.distinct)
          _ <- generateForOp(opsToHandle, domain, domain.flatten.typeCases, domainSpecific)

          // if extending a past operation chain, NO NEED for past ops
          // new data types need all operations
          _ <- generateForOp(domain.flatten.ops.distinct, domain, domain.typeCases, domainSpecific)

          // new operations (and producers from the past) need generation for types
          _ <- forEach(domain.former.flatMap(m => m.typeCases).distinct) { tpe => {
            generateForOpForType((nonPrimaryParentOps(domain, tpe) ++ domain.ops ++ domain.pastOperations.filter(op => op.isProducer(domain))).distinct,
              domain, tpe, domainSpecific)
          }}
//
//          _ <- generateForOp((domain.ops ++ domain.pastOperations.filter(op => op.isProducer(domain))).distinct,
//            domain, domain.former.flatMap(m => m.typeCases).distinct, domainSpecific)
        } yield()
    }

    for {
      _ <- domainSpecific.initialize(this)
      _ <- makeBase(model.baseDataType, Seq.empty)
      _ <- forEach(model.inChronologicalOrder) { m => {
           if (m.former.length > 1) {
             implementMerged(m)               // Merged are handled specially
           } else {
             implementInner(m)
           }
         }
      }
    } yield ()
  }

//  def implement2(gdomain: GenericModel, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
//
//    val domain = gdomain match {
//      case _:Model => gdomain.asInstanceOf[Model]
//      case _ => gdomain.linearize
//    }
//
//    /**
//     * For each operation must generate a sequence of classes, one per subtype.
//     * Must make sure we include ALL subtypes, not just ones from the past.
//     */
//    for {
//      _ <- registerTypeMapping(domain)
//      _ <- domainSpecific.initialize(this)
//
//      _ <- makeBase(domain.baseDataType, Seq.empty)
//
//     // required to do this first so interface/types are found. Dealing with MERGE is issue...
//      _ <- forEach (domain.inChronologicalOrder) { model => {
//        if (model.ops.nonEmpty) {
//         for {
//            _ <- registerTypeMapping(model) // this must be first SO Exp is properly bound within interfaces
//            _ <- registerTypeCases(model)   // handle DataType classes as well for interpreter
//            _ <- addIntermediateInterfaceToProject(model, domainSpecific, None)
//
//            _ <- generateForOp(model, model.ops, model, model.pastDataTypes,domainSpecific)
//
//            _ <- if (model.typeCases.nonEmpty) {
//              for {
//                _ <- forEach(model.toSeq.filter(mi => mi.ops.nonEmpty)) { mi =>
//                  generateForOp(mi, mi.ops, model, model.typeCases, domainSpecific)
//                }
//              } yield ()
//            } else {
//              Command.skip[ProjectContext]
//            }
//          } yield ()
//        } else {
//          // no operations. Must spit out new types and pull together all operations from the past.
//          for {
//            _ <- registerTypeCases(model)   // handle DataType classes as well for interpreter
//            _ <- generateForOp(model.lastModelWithOperation.head, model.flatten.ops, model, model.typeCases, domainSpecific)
//          } yield ()
//        }
//      }
//      }
//    } yield ()
//  }
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
