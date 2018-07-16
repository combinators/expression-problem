package shared.compilation

import scala.reflect.ClassTag

/**
  * Make it possible to register functions that take a CLASS and produce a specific type[C], which we can
  * make as perhaps a Seq[Statement]
  *
  * Copied AS IS from solitaire SPL example.
  *
  * @tparam C
  */
sealed trait CodeGeneratorRegistry[C] { self =>

  /** Given a domain element, return the translated result for that element (if it can find it). */
  def apply[E](domainElement : E): Option[C] =
    getGenerator(domainElement).map {
      case ((_, generator)) => generator(this, domainElement)
    }

  /** By default, if nothing added, get None. */
  protected def getGenerator[E](domainElement : E): Option[(Class[_ >: E], (CodeGeneratorRegistry[C], E) => C)] = None

  /** To add code generator, you get returned a new registry. */
  def addGenerator[E](generator: (CodeGeneratorRegistry[C], E) => C)(implicit gtag: ClassTag[E]): CodeGeneratorRegistry[C] = new CodeGeneratorRegistry[C] {
    override protected def getGenerator[E2](domainElement: E2): Option[(Class[_ >: E2], (CodeGeneratorRegistry[C], E2) => C)] = {
      (self.getGenerator(domainElement), gtag.runtimeClass.isAssignableFrom(domainElement.getClass)) match {
        case (Some((tpe, _)), true) if tpe.isAssignableFrom(gtag.runtimeClass) =>
          Some((gtag.runtimeClass.asInstanceOf[Class[_ >: E2]], generator.asInstanceOf[(CodeGeneratorRegistry[C], E2) => C]))
        case (None, true) =>
          Some((gtag.runtimeClass.asInstanceOf[Class[_ >: E2]], generator.asInstanceOf[(CodeGeneratorRegistry[C], E2) => C]))
        case (result, _) => result
      }
    }
  }

  /** Do the same thing by merging two repositories. */
  def merge(other: CodeGeneratorRegistry[C]): CodeGeneratorRegistry[C] = new CodeGeneratorRegistry[C] {
    override protected def getGenerator[E](domainElement: E): Option[(Class[_ >: E], (CodeGeneratorRegistry[C], E) => C)] = {
      (self.getGenerator(domainElement), other.getGenerator(domainElement)) match {
        case (Some((tpe, y)), Some((otherTpe, _))) if otherTpe.isAssignableFrom(tpe) => Some(tpe, y)
        case (Some((x, y)), None) => Some((x, y))
        case (_, Some((x, y))) => Some((x, y))
        case _ => None
      }
    }
  }
}

object CodeGeneratorRegistry {
  def apply[C]: CodeGeneratorRegistry[C] = new CodeGeneratorRegistry[C] {}
  def apply[C, E](generator: (CodeGeneratorRegistry[C], E) => C)(implicit tag: ClassTag[E]): CodeGeneratorRegistry[C] =
    this.apply[C].addGenerator(generator)
  def merge[C](registries: CodeGeneratorRegistry[C]*): CodeGeneratorRegistry[C] =
    registries.foldLeft(this.apply[C]){
      case (s, r) => s.merge(r)
    }
}