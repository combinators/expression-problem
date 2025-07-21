package org.combinators.ep.domain.matchers    /*DI:LI:AI*/

import scala.reflect.Selectable.reflectiveSelectable     // needed for Matchable

/** Provides programmatic pattern matching abstractions. */

/** Converts things of type `Source` into things of type `Content` if the source objects match a criterion. */
trait Matchable[Source, Content] {
  /** Returns `Some(content)` if the source object matches the desired criterion. */
  protected def toContent(source: Source): Option[Content]

  /** Transforms a function on `Content` by composing it with the result of `toContent`. */
  def matcher[Result](onMatch: Content => Result): PartialFunction[Source, Result] =
    Function.unlift(toContent).andThen(onMatch)

  /** Partially transforms a `Source` into `Content` by applying `toContent`. */
  def matcher: PartialFunction[Source, Content] = matcher(x => x)
}

/** Provides some standard Matchable instances. */
object Matchable {
  /** Returns a `Matchable` that only matches if the source object has the given name and the partial projection
   * function succeeds.
   */
  def named[Source <: {val name: String}, Content](
        name: String,
        project: PartialFunction[Source, Content]
      ): Matchable[Source, Content] = (source: Source) => if (source.name == name) project.lift(source)
  else None

  /** Returns a `Matchable` that only matches if the given projection function returns a singleton sequence. */
  def unary[Source, Content](project: PartialFunction[Source, Seq[Content]]): Matchable[Source, Content] =
    (source: Source) => project.lift.apply(source) match {
      case Some(Seq(content)) => Some(content)
      case _ => None
    }

  /** Returns a `Matchable` that only matches if the given partial projection function returns a sequence of exactly
   * two objects.
   */
  def binary[Source, Content](project: PartialFunction[Source, Seq[Content]]): Matchable[Source, (Content, Content)] =
    (source: Source) => project.lift.apply(source) match {
      case Some(Seq(left, right)) => Some(left, right)
      case _ => None
    }

  /** Returns a `Matchable` that matches anything by converting it into a sequence of contents. */
  def nary[Source, Content](project: PartialFunction[Source, Seq[Content]]): Matchable[Source, Seq[Content]] =
    (source: Source) => project.lift.apply(source)
}

