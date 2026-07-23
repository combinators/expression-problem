package org.combinators.equals

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.paradigm.AnyParadigm.syntax.forEach
import org.combinators.cogen.{Command, NameProvider, TypeRep, Understands}
import org.combinators.cogen.paradigm.{AnyParadigm, FindClass, ObjectOriented, ToTargetLanguageType}
import org.combinators.cogen.paradigm.control.Imperative
import org.combinators.cogen.paradigm.ffi.{Arrays, Booleans, Console, Equality}
import org.combinators.equals.ffi.BaseType

trait EqualsGenerator[AP <: AnyParadigm] {
  val paradigm: AP
  val names: NameProvider[paradigm.syntax.Name]
  val baseType: BaseType.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val impParadigm: Imperative.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val eqls: Equality.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val booleans: Booleans.WithBase[paradigm.MethodBodyContext, paradigm.type]
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  val arrays: Arrays.WithBase[paradigm.MethodBodyContext, paradigm.type]

  import paradigm.syntax._
  import paradigm.MethodBodyContext

  def createConstructor(domain:CompositeDataType): Generator[ooParadigm.ConstructorContext, Unit] = {
    import ooParadigm.constructorCapabilities._

    var mylist:Seq[(Name,Type)] = Seq.empty
    for {

      params <- forEach (domain.fields.toList) { (name,tpe)  =>
        for {
          pt <- convertToDomain[ooParadigm.ConstructorContext](tpe)
          pName <- freshName(names.mangle(name))
        } yield (pName, pt)
      }

      _ <- setParameters(params)

      args <- getArguments()
      _ <- forEach(args.zip(domain.fields.toList)) { case ((nm, tp, exp), original) => for {
          _ <- initializeField(names.mangle(original._1), exp)   // make sure to restore fieldname from original
        } yield ()
      }

    } yield ()
  }

  /**
   * public boolean equals (Object o) {
   *
   *    if (o instanceof CLASS) {
   *      CLASS other = (CLASS) o;
   *      // now field checks
   *      if (!this.f1.equals(other.f1)) { return false; }
   *      if (! F2) { return false; }
   *      ...
   *      return true;
   *    }
   *
   *    return false;
   * }
   */
  def generateSkeleton(domain:CompositeDataType): Generator[paradigm.ProjectContext, Unit] = {
    import ooParadigm.projectCapabilities._

    val makeClass: Generator[ooParadigm.ClassContext, Unit] = {
      import ooParadigm.classCapabilities._
      for {
        _ <- forEach (domain.fields.toList) { case (name, tpe) =>
          for {
            fieldType <- convertToDomain[ooParadigm.ClassContext](tpe)
            _ <- addField(names.mangle(name), fieldType)
          } yield ()
        }

        _ <- addConstructor(createConstructor(domain))
        _ <- addMethod(names.mangle("equals"), generateEquals(domain)) 
      } yield ()
    }

    addClassToProject(makeClass, names.mangle(domain.name))
  }

  def convertToDomain[Ctxt](dataType: DataType)(
    implicit canFindClass: Understands[Ctxt, FindClass[Name, Type]],
    canToTargetLanguage: Understands[Ctxt, ToTargetLanguageType[Type]]
  ) : Generator[Ctxt, Type] = {
    dataType match {
      case cdt: CompositeDataType =>  for {
        classType <- FindClass(Seq(names.mangle(cdt.name))).interpret(canFindClass)
      } yield classType

      case BuiltInDataType(tpeRep) => for {
        classType <- ToTargetLanguageType(tpeRep).interpret(canToTargetLanguage)
      } yield classType

    }
  }

//  def convertToDomainInClass(dataType: DataType): Generator[ooParadigm.ClassContext, Type] = {
//    dataType match {
//      case cdt: CompositeDataType => for {
//        classType <- ooParadigm.classCapabilities.findClass(names.mangle(cdt.name))
//      } yield classType
//
//      case BuiltInDataType(tpeRep) => for {
//        classType <- ooParadigm.classCapabilities.toTargetLanguageType(tpeRep)
//      } yield classType
//
//      case ArrayDataType(tpeRep) => for {
//        classType <- ooParadigm.classCapabilities.toTargetLanguageType(TypeRep.Array(tpeRep))
//      } yield classType
//    }
//  }
//
//  def convertToDomainInConstructor(dataType: DataType): Generator[ooParadigm.ConstructorContext, Type] = {
//    dataType match {
//      case cdt: CompositeDataType => for {
//        classType <- ooParadigm.constructorCapabilities.findClass(names.mangle(cdt.name))
//      } yield classType
//
//      case BuiltInDataType(tpeRep) => for {
//        classType <- ooParadigm.constructorCapabilities.toTargetLanguageType(tpeRep)
//      } yield classType
//
//      case ArrayDataType(tpeRep) => for {
//        classType <- ooParadigm.constructorCapabilities.toTargetLanguageType(TypeRep.Array(tpeRep))
//      } yield classType
//    }
//  }

  def ifBranch (domain:CompositeDataType, arg:Expression) : Generator[MethodBodyContext, Unit] = {
    import AnyParadigm.syntax._
    import ooParadigm.methodBodyCapabilities.canFindClassInMethod
    import paradigm.methodBodyCapabilities.canTransformTypeInMethodBody
    
    def returnFalse : Generator[MethodBodyContext, Unit] = {
      for {
        falseExpr <- booleans.booleanCapabilities.falseExp
        stmt <- impParadigm.imperativeCapabilities.returnStmt(falseExpr)
        _ <- paradigm.methodBodyCapabilities.addBlockDefinitions(Seq(stmt))
      } yield ()
    }

    for {
      classType <- convertToDomain[MethodBodyContext](domain)
      castExpr <- ooParadigm.methodBodyCapabilities.castObject(classType, arg)
      freshName <- paradigm.methodBodyCapabilities.freshName(names.mangle("other"))
      declVar <- impParadigm.imperativeCapabilities.declareVar(freshName, classType, Some(castExpr))

      // now for each field, add if statement with NEG forcing return false
      _ <- forEach (domain.fields.toList) { case (name,tpe) =>
        for {
          selfExpr <- ooParadigm.methodBodyCapabilities.selfReference()
          selfFieldExpr <- ooParadigm.methodBodyCapabilities.getMember(selfExpr, names.mangle(name))
          otherFieldExpr <- ooParadigm.methodBodyCapabilities.getMember(declVar, names.mangle(name))
          domainType <- convertToDomain[MethodBodyContext](tpe)

          // must handle arrays specially, not with == but with arrays capability

          eqlExpr <- eqls.equalityCapabilities.areEqual(domainType, selfFieldExpr, otherFieldExpr)
          notExpr <- booleans.booleanCapabilities.not(eqlExpr)

          failingIfExpr <- impParadigm.imperativeCapabilities.ifThenElse(notExpr, returnFalse, Seq.empty, None)
          _ <- paradigm.methodBodyCapabilities.addBlockDefinitions(Seq(failingIfExpr))
        } yield ()
      }

      trueExpr <- booleans.booleanCapabilities.trueExp
      retTrue <- impParadigm.imperativeCapabilities.returnStmt(trueExpr)
      _ <- paradigm.methodBodyCapabilities.addBlockDefinitions(Seq(retTrue))

    } yield ()
  }

  def generateEquals(domain:CompositeDataType) : Generator[MethodBodyContext, Option[Expression]] = {
    import ooParadigm.methodBodyCapabilities.canFindClassInMethod
    import paradigm.methodBodyCapabilities.canTransformTypeInMethodBody
    for {
      _ <- ooParadigm.methodBodyCapabilities.setOverride()   // since this is default behavior in OO languages
      boolType <- paradigm.methodBodyCapabilities.toTargetLanguageType(TypeRep.Boolean)
      _ <- paradigm.methodBodyCapabilities.setReturnType(boolType)
      objType <- paradigm.methodBodyCapabilities.toTargetLanguageType(BaseType.AnyTpe)
      _ <- paradigm.methodBodyCapabilities.setParameters(Seq((names.mangle("o"), objType)))

      falseExpr <- booleans.booleanCapabilities.falseExp
      trueExpr <- booleans.booleanCapabilities.trueExp
      retFalse <- impParadigm.imperativeCapabilities.returnStmt(falseExpr)
      retTrue <- impParadigm.imperativeCapabilities.returnStmt(trueExpr)
      classType <- convertToDomain[MethodBodyContext](domain)
      args <- paradigm.methodBodyCapabilities.getArguments()
      instOf <- ooParadigm.methodBodyCapabilities.instanceOfType(classType, args.head._3)

      ifExpr <- impParadigm.imperativeCapabilities.ifThenElse(instOf, ifBranch(domain, args.head._3), Seq.empty, None)
      _ <- paradigm.methodBodyCapabilities.addBlockDefinitions(Seq(ifExpr))
      _ <- paradigm.methodBodyCapabilities.addBlockDefinitions(Seq(retFalse))
    } yield None
  }
}

object EqualsGenerator {

  type Aux[AP <: AnyParadigm, OO <: ObjectOriented.WithBase[AP]] = EqualsGenerator[AP] {
    val paradigm: AP
    val ooParadigm: OO
  }

  def apply[AP <: AnyParadigm, OO[A <: AP] <: ObjectOriented.WithBase[A]](
          base: AP)(
              names: NameProvider[base.syntax.Name],
              baseType: BaseType.WithBase[base.MethodBodyContext, base.type],
              impParadigm: Imperative.WithBase[base.MethodBodyContext, base.type],
              eqls: Equality.WithBase[base.MethodBodyContext, base.type],
              booleans: Booleans.WithBase[base.MethodBodyContext, base.type],
              arrays: Arrays.WithBase[base.MethodBodyContext, base.type],
              ooParadigm: OO[base.type]): Aux[base.type, ooParadigm.type] = {
    val b: base.type = base
    val oo: ooParadigm.type = ooParadigm
    val namesIn: names.type = names
    val baseTypeIn: baseType.type = baseType
    val impParadigmIn: impParadigm.type = impParadigm
    val eqlsIn: eqls.type = eqls
    val booleansIn: booleans.type = booleans
    val arraysIn: arrays.type = arrays

    case class T(
          val paradigm: b.type,
          val names: namesIn.type,
          val baseType: baseTypeIn.type,
          val impParadigm: impParadigmIn.type,
          val eqls: eqlsIn.type,
          val booleans: booleansIn.type,
          val arrays: arraysIn.type,
          val ooParadigm: oo.type
    ) extends EqualsGenerator[b.type]

    T(b, namesIn, baseTypeIn, impParadigmIn, eqlsIn, booleansIn, arrays, oo)
  }
}