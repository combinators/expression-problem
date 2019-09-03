package org.combinators.ep.generator.paradigm

import org.combinators.ep.generator.{AbstractSyntax, Command, Understands}
import org.combinators.ep.generator.Command.Generator

case class AddType[TypeContext](name: String, tpeGen: Generator[TypeContext, Unit]) extends Command {
  type Result = Unit
}

case class AddTypeConstructor[Type](name: String, parameters: Seq[Type]) extends Command {
  type Result = Unit
}

case class PatternMatch[MethodBodyContext, Expression](
    onValue: Expression,
    options: (String, Seq[Expression]) => Generator[MethodBodyContext, Expression]
  ) extends Command {
  type Result = Expression
}

case class InstantiateType[Type, Expression](
    tpe: Type,
    constructor: String,
    arguments: Seq[Expression]
  ) extends Command {
  type Result = Expression
}


abstract class Functional[S <: AbstractSyntax](override val syntax: S) extends AnyParadigm[S](syntax) {
  import syntax._

  type TypeContext

  implicit val canAddTypeInCompilationUnit: Understands[CompilationUnitContext, AddType[TypeContext]]
  implicit val canAddMethodInCompilationUnit: Understands[CompilationUnit, AddMethod[MethodBodyContext, Type, Expression]]

  implicit val canAddTypeConstructorInType: Understands[TypeContext, AddTypeConstructor[Type]]

  implicit val canPatternMatchInMethod: Understands[MethodBodyContext, PatternMatch[MethodBodyContext, Expression]]
  implicit val canInstantiateTypeInMethod: Understands[MethodBodyContext, InstantiateType[Type, Expression]]
}
