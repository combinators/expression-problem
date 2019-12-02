package org.combinators.ep.approach.oo

import org.combinators.ep.domain.Model
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.Command.{Generator, _}
import org.combinators.ep.generator._
import org.combinators.ep.generator.communication._
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax._
import org.combinators.ep.generator.paradigm._


// copied from visitor to start
sealed trait Interpreter extends OOApproachImplementationProvider with OperationInterfaceChain with FieldDefinition with FactoryConcepts {
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
      rt <- findClass(names.mangle(names.conceptNameOf(tpeCase))) // this will be to reference a factory method, not 'new'

      _ <- resolveAndAddImport(rt)
      res <- instantiateObject(rt, args)
    } yield res
  }

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
  override def factoryNameDataTypeCase(model:Option[Model] = None, tpeCase:DataTypeCase) : Name = {
    names.addSuffix(names.mangle(opsName(model.get.ops)), names.conceptNameOf(model.get.baseDataType))
  }

  /**
   * Standard factory name for a dataTypeCase ignores the tpeCase and only focuses on the model
   *
   * {{{
   *   EvalAdd
   * }}}
   *
   * Model is passed in should it become necessary to be overridden more specifically
   *
   * @param tpeCase    DataTypeCase for which a factory is desired.
   * @return
   */
  override def factoryInstanceDataTypeCase(model:Option[Model] = None, tpeCase:DataTypeCase) : Name = {
    names.addSuffix(names.mangle(opsName(model.get.ops)), names.conceptNameOf(tpeCase))
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
   * This is what I generated before, one for each subtype and the operations defined within it.  Have to be careful
   * to keep extension chain going properly when there are prior classes available
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
   *
   * // //public class PrettypAdd extends EvalIdzAdd implements PrettypExp {
   *
   * @param model
   * @param ops
   * @return
   */
  def makeClassForCase(model: Model, ops: Seq[Operation], tpeCase: DataTypeCase, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ClassContext, Unit] = {
    import classCapabilities._
    import ooParadigm.projectCapabilities._

    def addParentClass(): Generator[ClassContext, Unit] = if (!shouldAddParent()) {
      Command.skip[ClassContext]
    } else {
      val lastOpModel = model.last.get.lastModelWithOperation.get
      val priorClass = baseInterfaceNamesPrefix(lastOpModel.ops, names.mangle(names.conceptNameOf(tpeCase)))
      for {
        priorType <- findClass(priorClass)
        _ <- resolveAndAddImport(priorType)
        _ <- addParent(priorType)
      } yield ()
    }

    // Should add parent when
    //     There exists a prior operation for this model (since it was there)
    def shouldAddParent(): Boolean = if (model.last == Some(model.base) || model.last.isEmpty) {
      false
    } else {
      if (model.last.get.lastModelWithOperation.isEmpty) {
        false
      } else {
       true
      }
    }

    for {
       pt <- toTargetLanguageType(TypeRep.DataType(model.baseDataType))
       _ <- resolveAndAddImport(pt)
       _ <- addImplemented(pt)

       _ <- addParentClass()
       _ <- forEach(tpeCase.attributes) { att => makeField(att) }
       _ <- addConstructor(makeConstructor(tpeCase, shouldAddParent()))
       _ <- forEach (ops) { op =>
         addMethod(names.mangle(names.instanceNameOf(op)), makeImplementation(model.baseDataType, tpeCase, op, domainSpecific))
       }
    } yield ()
  }


  def generateForOp(model:Model, ops:Seq[Operation], allTypes:Seq[DataTypeCase], isBase:Boolean, domainSpecific: EvolutionImplementationProvider[this.type]) : Generator[ProjectContext, Unit] = {
    val combinedOps:String = ops.sortWith(_.name < _.name).map(op => names.conceptNameOf(op)).mkString("")
    import ooParadigm.projectCapabilities._

    for {
      _ <- forEach (allTypes) { tpeCase => {
        // register the *LAST* model -- either the one that defines the tpeCase or the one that defines the operation
        val modelToUse = if (model.findTypeCase(tpeCase).isEmpty || model.findTypeCase(tpeCase).get.findOperation(ops.head).isEmpty) {
          model.findOperation(ops.head).get
        } else {
          model.findTypeCase(tpeCase).get
        }
        for {
          _ <- registerTypeMapping(modelToUse)
          _ <- addClassToProject(names.addSuffix(names.mangle(combinedOps), names.conceptNameOf(tpeCase)), makeClassForCase(modelToUse, ops, tpeCase, domainSpecific))
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
   *     public static EvalIdzExp Mult(EvalIdzExp left, EvalIdzExp right) {
   *         return new EvalIdzMult(left, right);
   *     }
   *
   *     public static EvalIdzExp Divd(EvalIdzExp left, EvalIdzExp right) {
   *         return new EvalIdzDivd(left, right);
   *     }
   *
   *     public static EvalIdzExp Sub(EvalIdzExp left, EvalIdzExp right) {
   *         return new EvalIdzSub(left, right);
   *     }
   *
   *     public static EvalIdzExp Lit(Double value) {
   *         return new EvalIdzLit(value);
   *     }
   *
   *     public static EvalIdzExp Add(EvalIdzExp left, EvalIdzExp right) {
   *         return new EvalIdzAdd(left, right);
   *     }
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
        _ <- forEach(model.pastDataTypes) {
          tpe =>
            addMethod(names.mangle(names.conceptNameOf(tpe)), createFactoryDataTypeCase(model, tpe, isStatic=true))
        }
      } yield ()
    }

    import paradigm.projectContextCapabilities._
    for {
      _ <- debug ("facName:" + opsName(model.ops))
      _ <- addClassToProject(names.addSuffix(names.mangle(opsName(model.ops)), "Factory"), factoryClass(model))
    } yield ()
  }

  /** For Trivially, the covariant type needs to be selected whenever a BaseType in the domain is expressed. */
  def domainTypeLookup[Ctxt](covariantType: Name)(implicit canFindClass: Understands[Ctxt, FindClass[Name, Type]]): Generator[Ctxt, Type] = {
    FindClass(covariantType).interpret(canFindClass)
  }

  /** What model is delivered has operations which is essential for the mapping. */
  override def registerTypeMapping(model: Model): Generator[ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._
    import paradigm.projectContextCapabilities._
    import ooParadigm.methodBodyCapabilities._
    import ooParadigm.classCapabilities._
    import ooParadigm.constructorCapabilities._
    import paradigm.testCapabilities._
    import ooParadigm.testCapabilities._

    // must always be an operation in FIRST evolution
    val baseInterface = baseInterfaceNames(model.lastModelWithOperation.get, model.lastModelWithOperation.get.ops)
    val dtpeRep = TypeRep.DataType(model.baseDataType)
    for {
      _ <- addTypeLookupForMethods(dtpeRep, domainTypeLookup(baseInterface))
      _ <- addTypeLookupForClasses(dtpeRep, domainTypeLookup(baseInterface))
      _ <- addTypeLookupForConstructors(dtpeRep, domainTypeLookup(baseInterface))
    } yield ()
  }

  def implement(domain: Model, domainSpecific: EvolutionImplementationProvider[this.type]): Generator[ProjectContext, Unit] = {

    /**
     * For each operation must generate a sequence of classes, one per subtype.
     * Must make sure we include ALL subtypes, not just ones from the past.
     */
    val flat = domain.flatten
    for {
      _ <- registerTypeMapping(domain)
      _ <- domainSpecific.initialize(this)

      _ <- makeBase(domain.baseDataType, Seq.empty)

      _ <- forEach (domain.inChronologicalOrder.filter(m => m.ops.nonEmpty)) {
        m => for {
          _ <- generateFactory(m)
        } yield ()
      }

      // for each model with operations, must create cross-product of all past datatypes
      _ <- forEach (domain.inChronologicalOrder.filter(m => m.ops.nonEmpty)) { m => generateForOp(m, m.ops, m.pastDataTypes, m.isBase, domainSpecific) }

      // for any model that has data types have to check for PAST operations that were defined.
//      _ <- forEach (domain.inChronologicalOrder.filter(m => m.ops.isEmpty && m.typeCases.nonEmpty)) { m =>
//        generateForOp(m.lastModelWithOperation.get, m.lastModelWithOperation.get.ops, m.typeCases, false, domainSpecific) }

      _ <- forEach (domain.inChronologicalOrder.filter(m => m.ops.isEmpty && m.typeCases.nonEmpty)) { m => {
        forEach(m.toSeq.filter(mi => mi.ops.nonEmpty)) { mi =>
          generateForOp(mi, mi.ops, m.typeCases, false, domainSpecific)
        }
      }
      }


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
