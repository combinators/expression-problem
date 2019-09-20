package org.combinators.ep.generator

case class FreshNameProvider[Name](pushName: (Name, Int) => Name, used: Set[Name] = Set.empty[Name]) {

  def markUsed(name: Name): FreshNameProvider[Name] = {
    FreshNameProvider(pushName, used + name)
  }

  def markUnused(name: Name): FreshNameProvider[Name] = {
    FreshNameProvider(pushName, used - name)
  }

  def freshNameBasedOn(name: Name): (Name, FreshNameProvider[Name]) = {
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