package org.combinators.helloworld

import org.combinators.cogen.InstanceRep
import org.combinators.cogen.paradigm.{AddImport, AnyParadigm, ObjectOriented, ResolveImport}
import org.combinators.cogen.Command._
import org.combinators.cogen.{NameProvider, Understands}

/** Attempt to provide a hello world generator. */
trait HelloWorldProvider {
  val paradigm: AnyParadigm
  val names: NameProvider[paradigm.syntax.Name]
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]
  import paradigm._
  import syntax._


  /** Available in any Context that can ResolveImport and AddImport. */
  def resolveAndAddImport[Context, Elem](elem: Elem)
    (implicit
      canResolveImport: Understands[Context, ResolveImport[Import, Elem]],
      canAddImport: Understands[Context, AddImport[Import]]
    ) : Generator[Context, Unit] = {
    ResolveImport[Import, Elem](elem).interpret.flatMap(imp => imp.map(AddImport(_).interpret).getOrElse(skip))
  }

  /** Converts a Scala model of an instance of any representable type into code. */
  def reify(inst: InstanceRep): Generator[MethodBodyContext, Expression] = {
    (inst.tpe, inst.inst) match {
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

  /** Entry point into code generation. */
  def implement(): Generator[ProjectContext, Unit]

  /** Define standard test name. */
  def testCaseName:Name = {
    names.mangle("Test")
  }

}

