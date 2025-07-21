package org.combinators.cogen

case class FreshNameProvider[Name](
  pushName: (Name, Int) => Name,
  parent: Option[FreshNameProvider[Name]] = None,
  used: Set[Name] = Set.empty[Name]
) {

  def isUsed(name: Name): Boolean = {
    used(name) || parent.exists(_.isUsed(name))
  }

  def markUsed(name: Name): FreshNameProvider[Name] = {
    copy(used = used + name)
  }

  def markUnused(name: Name): FreshNameProvider[Name] = {
    copy(used = used - name, parent = parent.map(_.markUnused(name)))
  }

  def freshNameBasedOn(name: Name): (Name, FreshNameProvider[Name]) = {
    if (!isUsed(name)) {
      (name, markUsed(name))
    } else {
      var suffix = 0
      var pushed = pushName(name, suffix)
      while (isUsed(pushed)) {
        suffix += 1
        pushed = pushName(name, suffix)
      }
      (pushed, markUsed(pushed))
    }
  }

  def pushContext: FreshNameProvider[Name] =
    FreshNameProvider(pushName, Some(this))

  def popContext: FreshNameProvider[Name] =
    parent.getOrElse(FreshNameProvider(pushName))
}