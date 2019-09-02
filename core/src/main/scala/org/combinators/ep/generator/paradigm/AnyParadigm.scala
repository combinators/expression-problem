package org.combinators.ep.generator.paradigm

import org.combinators.ep.domain.abstractions.{DataType, DataTypeCase, TypeRep}
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.generator.{AbstractSyntax, Command, Understands}
import Command._
import cats.implicits._

/** Adds a named compilation unit. */
case class AddCompilationUnit[CompilationUnitContext](name: String) extends Command {
  type Result = CompilationUnitContext
}

/** Adds the given import. */
case class AddImport[Import](imp: Import) extends Command {
  type Result = Unit
}

/** Adds the given definitions to a block of statements. */
case class AddBlockDefinitions[Statement](definitions: Seq[Statement]) extends Command {
  type Result = Unit
}

/** Uses the given result as the result of a function or method. */
case class Return[Expression, Statement](result: Expression) extends Command {
  type Result = Seq[Statement]
}

/** Translates the Scala representation of a type to target language specific code for referring to it. */
case class ToTargetLanguageType[Type](tpe: TypeRep) extends Command {
  type Result = Type
}

/** Adds a method. */
case class AddMethod[MethodBodyContext, Type](
   name: String,
   returnType: Type,
   parameters: Seq[(String, Type)],
   isPublic: Boolean = true
  ) extends Command {
  type Result = MethodBodyContext
}

abstract class AnyParadigm[S <: AbstractSyntax](val syntax: S) {
  import syntax._

  type ProjectContext
  type CompilationUnitContext
  type MethodBodyContext

  implicit val canAddCompilationUnitInProject: Understands[ProjectContext, AddCompilationUnit[CompilationUnitContext]]
  implicit val canAddImportInCompilationUnit: Understands[CompilationUnitContext, AddImport[Import]]
  implicit val canAddImportInMethodBody: Understands[MethodBodyContext, AddImport[Import]]
  implicit val canAddBlockDefinitionsInMethodBody: Understands[MethodBodyContext, AddBlockDefinitions[Statement]]
  implicit val canReturnInMethodBody: Understands[MethodBodyContext, Return[Expression, Statement]]
  implicit val canTransformTypeInMethodBody: Understands[MethodBodyContext, ToTargetLanguageType[Type]]

  /** Creates an empty project */
  def emptyProject(name: String): ProjectContext

  /** Returns code to instantiate the given data type case, filling in `args` for its parameters. */
  def instantiate[Expression](baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression]

  /** Returns code to instantiate the given Scala model of a domain specific type. */
  def instantiate(baseType: DataType, inst: DataTypeInstance): Generator[MethodBodyContext, Expression] = {
    for {
      attributeInstances <- inst.attributeInstances.toList.map(reify).sequence[Generator[MethodBodyContext, *], Expression]
      result <- instantiate(baseType, inst.tpeCase, attributeInstances: _*)
    } yield result
  }

  /** Converts the given Scala value into code representing it in the target language. */
  def reify[T](tpe: TypeRep.OfHostType[T], value: T): Generator[MethodBodyContext, Expression]

  /** Converts a Scala model of an instance of any representable type into code. */
  def reify(inst: InstanceRep): Generator[MethodBodyContext, Expression] = {
    (inst.tpe, inst.inst) match {
      case (TypeRep.DataType(baseTpe), domInst: DataTypeInstance) => instantiate(baseTpe, domInst)
      case (tpe: TypeRep.OfHostType[_], inst) => reify(tpe, inst)
      case _ => throw new scala.NotImplementedError(s"No rule to compile instantiations of ${inst.tpe}.")
    }
  }
}

