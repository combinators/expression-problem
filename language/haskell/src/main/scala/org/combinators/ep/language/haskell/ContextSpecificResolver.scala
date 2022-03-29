package org.combinators.ep.language.haskell   /*DI:LD:AI*/

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.generator.Command
import org.combinators.ep.generator.Command.Generator
import Syntax.default._

case class ContextSpecificResolver(
  _methodTypeResolution: ContextSpecificResolver => TypeRep => Generator[MethodBodyCtxt, ast.Type],
  _typeTypeResolution: ContextSpecificResolver => TypeRep => Generator[TypeCtxt, ast.Type],
  _reificationInMethod: ContextSpecificResolver => InstanceRep => Generator[MethodBodyCtxt, Expression],
  _tpeImportResolution: ContextSpecificResolver => ast.Type => Option[Import],
  _termImportResolution: ContextSpecificResolver => Expression => Option[Import],
  _instantiationOverride: ContextSpecificResolver => (ast.Type, Seq[Expression]) => (ast.Type, Seq[Expression]),
  resolverInfo: Set[Any] = Set.empty
) {
  def methodTypeResolution(tpeRep: TypeRep): Generator[MethodBodyCtxt, ast.Type] =
    _methodTypeResolution(this)(tpeRep)
  def typeTypeResolution(tpeRep: TypeRep): Generator[TypeCtxt, ast.Type] =
    _typeTypeResolution(this)(tpeRep)
  def reificationInMethod(instRep: InstanceRep): Generator[MethodBodyCtxt, Expression] =
    _reificationInMethod(this)(instRep)
  def importResolution(tpe: ast.Type): Option[Import] =
    _tpeImportResolution(this)(tpe)
  def importResolution(term: Expression): Option[Import] =
    _termImportResolution(this)(term)
  def instantiationOverride(tpe: ast.Type, args: Seq[Expression]): (ast.Type, Seq[Expression]) =
    _instantiationOverride(this)(tpe,args)

  def addInfo(info: Any): ContextSpecificResolver = copy(resolverInfo = resolverInfo + info)
}

object ContextSpecificResolver {
  def updateResolver
    (rep: TypeRep, translateTo: ast.Type, extraImport: Option[Import] = None)
      (reification: rep.HostType => ast.Expression): ContextSpecificResolver => ContextSpecificResolver =
    resolver => {
      def addResolutionType[Ctxt](
        targetType: ast.Type,
        toResolution: ContextSpecificResolver => TypeRep => Generator[Ctxt, ast.Type]
      ): ContextSpecificResolver => TypeRep => Generator[Ctxt, ast.Type] = k => {
        case r if r == rep => Command.lift(targetType)
        case other => toResolution(k)(other)
      }

      def addReification[Ctxt](
        reify: ContextSpecificResolver => InstanceRep => Generator[Ctxt, Expression]
      ): ContextSpecificResolver => InstanceRep => Generator[Ctxt, Expression] = k => {
        case instRep if instRep.tpe == rep =>
          Command.lift(reification(instRep.inst.asInstanceOf[rep.HostType]))
        case other => reify(k)(other)
      }

      def addExtraTypeImport(
        importResolution: ContextSpecificResolver => ast.Type => Option[Import]
      ): ContextSpecificResolver => ast.Type => Option[Import] = k =>  {
        case r if r == translateTo => extraImport
        case other => importResolution(k)(other)
      }

      resolver.copy(
        _methodTypeResolution =
          addResolutionType(
            translateTo,
            resolver._methodTypeResolution
          ),
        _typeTypeResolution =
          addResolutionType(
            translateTo,
            resolver._typeTypeResolution
          ),
        _reificationInMethod = addReification(resolver._reificationInMethod),
        _tpeImportResolution = addExtraTypeImport(resolver._tpeImportResolution)
      )
    }
}