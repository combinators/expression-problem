package ep.domain  /*DI:LI:AI*/

/** Enables access to specific domain. */
class WithDomain[+E <: BaseDomain](val domain:E) { }   // a covariant class in Scala

/** Every individual evolution extends this Trait to have access to the domain.Model. */
trait Evolution {

  val domain: ModelDomain                             // Every evolution has associated domain.
  def getModel: domain.Model

}
