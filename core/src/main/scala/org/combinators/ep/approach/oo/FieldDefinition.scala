package org.combinators.ep.approach.oo

import org.combinators.ep.domain.abstractions.Attribute
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.ObjectOriented
import org.combinators.ep.generator.ApproachImplementationProvider

/**
 * Ability to create fields for given Attribute within an existing Class
 *
 *  {{{
 *     public interface Exp {}    // marker interface
 *
 *
 *     public interface ExpEval extends Exp {
 *       ...
 *     }
 *
 *     public interface ExpPrettyp extends ExpEval {
 *        ...
 *     }
 * }}}
 *
 * Where 'Exp' comes from the BaseDataType of the domain.
 */
trait FieldDefinition extends ApproachImplementationProvider with SharedOO {
  val ooParadigm: ObjectOriented.WithBase[paradigm.type]

  import ooParadigm._
  import paradigm._
  import syntax._

  /** Make a field from an attribute in the given class.  If the type needs to be different from default, then overload attToType */
  def makeField(att: Attribute): Generator[ClassContext, Type] = {
    import ooParadigm.classCapabilities._
    for {
      ft <- toTargetLanguageType(att.tpe)
      _ <- resolveAndAddImport(ft)
      _ <- addField(names.mangle(names.instanceNameOf(att)), ft)
    } yield ft
  }
}

