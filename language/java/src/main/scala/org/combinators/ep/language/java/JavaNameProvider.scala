package org.combinators.ep.language.java

import org.combinators.ep.generator.NameProvider

/**
 * Each language-specific generator requires a language-specific name mangler
 */
object JavaNameProvider extends NameProvider {
  /** Mangles `name` according to language specific rules. */
  override def mangle(name: String): String = name
}
