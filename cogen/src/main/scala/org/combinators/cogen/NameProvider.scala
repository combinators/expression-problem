package org.combinators.cogen

/** Provides mangled names for domain entities. */
abstract class NameProvider[Name] {
  /** Mangles `name` according to language specific rules, with set . */
  def mangle(name: String): Name

  /** Adds a prefix to the given name. The new name will be mangled if necessary. */
  def addPrefix(prefix: String, name: Name): Name

  /** Adds a suffix to the given name. The new name will be mangled if necessary. */
  def addSuffix(name: Name, suffix: String): Name
}
