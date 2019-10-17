package org.combinators.ep.approach.oo

import org.combinators.ep.domain.Model
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command._
import org.combinators.ep.generator._
import org.combinators.ep.generator.communication._
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax._
import org.combinators.ep.generator.paradigm._


// copied from visitor to start
sealed trait Interpreter extends ApproachImplementationProvider {
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

  // this is commonly copied from numerous ones (Visitor/Traditional) seems to be best to lift up to some other place
  def makeSignature(op: Operation): Generator[MethodBodyContext, Unit] = {
    import paradigm.methodBodyCapabilities._

    for {
      rt <- toTargetLanguageType(op.returnType)
      _ <- resolveAndAddImport(rt)
      _ <- setReturnType(rt)
      params <- forEach (op.parameters) { param: Parameter =>
          for {
            pt <- toTargetLanguageType(param.tpe)
            _ <- resolveAndAddImport(pt)
            pName <- freshName(names.mangle(param.name))
          } yield (pName, pt)
        }
      _ <- setParameters(params)
    } yield ()
  }

  def makeBase(tpe: DataType, ops: Seq[Operation]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
        import classCapabilities._
        for {
          _ <- setAbstract()
          _ <- forEach(ops) { op => addAbstractMethod(names.mangle(names.instanceNameOf(op)), makeSignature(op)) }
          _ <- debug("makeBase")
        } yield ()
      }
    addClassToProject(names.mangle(names.conceptNameOf(tpe)), makeClass)
  }

  def makeField(att: Attribute): Generator[ClassContext, Unit] = {
    import ooParadigm.classCapabilities._
    for {
      ft <- toTargetLanguageType(att.tpe)
      _ <- resolveAndAddImport(ft)
      _ <- addField(names.mangle(names.instanceNameOf(att)), ft)
    } yield ()
  }

  def makeImplementation(
      tpe: DataType,
      tpeCase: DataTypeCase,
      op: Operation,
      domainSpecific: EvolutionImplementationProvider[this.type]
    ): Generator[MethodBodyContext, Option[Expression]] = {
    import ooParadigm.methodBodyCapabilities._
    import paradigm.methodBodyCapabilities._
    for {
      _ <- makeSignature(op)
      thisRef <- selfReference()
      attAccessors: Seq[Expression] <- forEach (tpeCase.attributes) { att =>
          getMember(thisRef, names.mangle(names.instanceNameOf(att)))
        }
      atts = tpeCase.attributes.zip(attAccessors).toMap
      allArgs <- getArguments()
      args = op.parameters.zip(allArgs).map { case (param, (_, _, exp)) => (param, exp) }.toMap
      result <-
        domainSpecific.logic(this)(
          ReceivedRequest(
            tpe,
            tpeCase,
            thisRef,
            atts,
            Request(op, args)
          )
        )
    } yield result
  }

  def makeConstructor(tpeCase: DataTypeCase): Generator[ConstructorContext, Unit] = {
    import ooParadigm.constructorCapabilities._
    for {
      params <- forEach (tpeCase.attributes) { att: Attribute =>
          for {
            at <- toTargetLanguageType(att.tpe)
            _ <- resolveAndAddImport(at)
            pName <- freshName(names.mangle(names.instanceNameOf(att)))
          } yield (pName, at)
        }
      _ <- setParameters(params)
      args <- getArguments()
      _ <- forEach(tpeCase.attributes.zip(args)) { case (att, (_, _, exp)) =>
        initializeField(names.mangle(names.instanceNameOf(att)), exp)
      }
    } yield ()
  }

  def makeDerived(tpe: DataType, tpeCase: DataTypeCase, ops: Seq[Operation], domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    val makeClass: Generator[ClassContext, Unit] = {
      import classCapabilities._
      for {
        pt <- findClass(names.mangle(names.conceptNameOf(tpe)))
        //pt <- toTargetLanguageType(TypeRep.DataType(tpe))
        _ <- resolveAndAddImport(pt)
        _ <- addParent(pt)
        _ <- forEach (tpeCase.attributes) { att => makeField(att) }
        _ <- addConstructor(makeConstructor(tpeCase))
        _ <- forEach (ops) { op =>
            addMethod(names.mangle(names.instanceNameOf(op)), makeImplementation(tpe, tpeCase, op, domainSpecific))
          }
      } yield ()
    }
    addClassToProject(names.mangle(names.conceptNameOf(tpeCase)), makeClass)
  }

  def domainTypeLookup[Ctxt](dtpe: DataType)(implicit canFindClass: Understands[Ctxt, FindClass[Name, Type]]): Generator[Ctxt, Type] = {
    FindClass(names.mangle(names.conceptNameOf(dtpe))).interpret(canFindClass)
  }

  /**
   * When trying to find an import that is needed for a class, there is a lookup function and ResolveImports uses this lookup
   * to be able to import aclass.
   * @param domain
   * @return
   */
  def initializeApproach(domain: Model): Generator[ProjectContext, Unit] = {
    import paradigm.projectContextCapabilities._
    import ooParadigm.projectCapabilities._
    import ooParadigm.methodBodyCapabilities._            // NOTE: DO NOT REMOVE SINCE USED BY BELOW AddTypeLookup
    import ooParadigm.classCapabilities._
    import ooParadigm.constructorCapabilities._
    val dtpeRep = TypeRep.DataType(domain.baseDataType)
    for {
      _ <- addTypeLookupForMethods(dtpeRep, domainTypeLookup(domain.baseDataType))
      _ <- addTypeLookupForClasses(dtpeRep, domainTypeLookup(domain.baseDataType))
      _ <- addTypeLookupForConstructors(dtpeRep, domainTypeLookup(domain.baseDataType))
    } yield ()
  }

  def makeBaseExtensions(model:Model): Generator[ProjectContext, Unit] = {
    val allTypes = model.pastDataTypes
    val isBase: Boolean = model.base.equals(model)
    generateForOp(model, model.ops, allTypes, isBase)
  }


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
      _ <- debug("makeClassForCase")
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



//    allTypes.zip(classes).foreach ()
//    val name = names.conceptNameOf(exp)
//    addClassToProject(names.mangle(combinedOps + name), makeClass)

  }

  def implement(domain: Model, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {

    /**
     * For each operation must generate a sequence of classes, one per subtype.
     * Must make sure we include ALL subtypes, not just ones from the past.
     */
    for {
      _ <- initializeApproach(domain)
      _ <- domainSpecific.initialize(this)
      _ <- makeBase(domain.baseDataType, domain.ops)
      _ <- forEach (domain.inChronologicalOrder.filter(m => m.ops.nonEmpty)) { m =>
        makeBaseExtensions(m)
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
