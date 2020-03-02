package org.combinators.ep.approach.oo

import org.combinators.ep.domain.abstractions.Attribute
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.ObjectOriented
import org.combinators.ep.generator.ApproachImplementationProvider

/**
 * Ability to create fields for given Attribute within an existing Class
 */
trait FieldDefinition extends ApproachImplementationProvider {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]

  import ooParadigm._
  import paradigm._
  import syntax._

  /** Make a field from an attribute in the given class.  If the type needs to be different from default, then register Types accordingly. */
  def makeField(att: Attribute): Generator[ClassContext, Type] = {
    import ooParadigm.classCapabilities._
    for {
      ft <- toTargetLanguageType(att.tpe)
      _ <- debug("FT:" + ft)
      _ <- resolveAndAddImport(ft)
      _ <- addField(names.mangle(names.instanceNameOf(att)), ft)
    } yield ft
  }
}
