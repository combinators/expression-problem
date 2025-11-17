package org.combinators.bottomUp.twosequences.testLCS

import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.generator.Command._
import org.combinators.ep.generator.paradigm.AnyParadigm.syntax._
import org.combinators.ep.generator.paradigm.{AddImport, AnyParadigm, ObjectOriented, ResolveImport}
import org.combinators.ep.generator.{NameProvider, Understands}

trait LongestCommonSubsequenceProvider {
  val paradigm: AnyParadigm
  val names: NameProvider[paradigm.syntax.Name]
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]

  import paradigm._
  import syntax._

  def instantiate(baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression]

  def instantiate(baseType: DataType, inst: DataTypeInstance): Generator[MethodBodyContext, Expression] = {
    for {
      attributeInstances <- forEach(inst.attributeInstances) { ati => reify(ati) }
      result <- instantiate(baseType, inst.tpeCase, attributeInstances: _*)
    } yield result
  }

  def resolveAndAddImport[Context, Elem](elem: Elem)
                                        (implicit
                                         canResolveImport: Understands[Context, ResolveImport[Import, Elem]],
                                         canAddImport: Understands[Context, AddImport[Import]]
                                        ): Generator[Context, Unit] = {
    ResolveImport[Import, Elem](elem).interpret.flatMap(imp => imp.map(AddImport(_).interpret).getOrElse(skip))
  }

  def reify(inst: InstanceRep): Generator[MethodBodyContext, Expression] = {
    (inst.tpe, inst.inst) match {
      case (TypeRep.DataType(baseTpe), domInst: DataTypeInstance) => instantiate(baseTpe, domInst)

      case (tpe, inst) =>
        import paradigm.methodBodyCapabilities._

        for {
          resTy <- toTargetLanguageType(tpe)
          _ <- resolveAndAddImport(resTy)
          res <- methodBodyCapabilities.reify[tpe.HostType](tpe, inst.asInstanceOf[tpe.HostType])
        } yield res

      case _ => throw new scala.NotImplementedError(s"No rule to compile instantiations of ${inst.tpe}.")
    }
  }
}