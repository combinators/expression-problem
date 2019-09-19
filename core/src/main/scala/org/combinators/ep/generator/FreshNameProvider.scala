package org.combinators.ep.generator

case class FreshNameProvider(pushName: (String, Int) => String, used: Set[String] = Set.empty) {

  def markUsed(name: String): FreshNameProvider = {
    FreshNameProvider(pushName, used + name)
  }

  def markUnused(name: String): FreshNameProvider = {
    FreshNameProvider(pushName, used - name)
  }

  def freshNameBasedOn(name: String): (String, FreshNameProvider) = {
    if (!used(name)) {
      (name, markUsed(name))
    } else {
      var suffix = 0
      var pushed = pushName(name, suffix)
      while (used(pushed)) {
        suffix += 1
        pushed = pushName(name, suffix)
      }
      (pushed, markUsed(pushed))
    }
  }
}