package org.combinators.ep.generator

case class FreshNameProvider(used: Set[String] = Set.empty) {

  def markUsed(name: String): FreshNameProvider = {
    FreshNameProvider(used + name)
  }

  def markUnused(name: String): FreshNameProvider = {
    FreshNameProvider(used - name)
  }

  def freshNameBasedOn(name: String): (String, FreshNameProvider) = {
    if (!used(name)) {
      (name, markUsed(name))
    } else {
      var suffix = 0
      while (used(name + suffix)) suffix += 1
      (name + suffix, markUsed(name + suffix))
    }
  }
}