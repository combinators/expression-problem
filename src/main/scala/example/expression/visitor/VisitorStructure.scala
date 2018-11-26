package example.expression.visitor    /*DI:LI:AD*/

import example.expression.generator.LanguageIndependentGenerator

/**
  * Provides language-independent and domain-independent interface to access the
  * structural element of the Visitor design pattern, which commonly appears in
  * EP solutions.
  *
  * Extends LanguageIndependentGenerator to access base concepts
  */
trait VisitorStructure extends LanguageIndependentGenerator {

  type Declaration       /** Base concept for the representation of a declaration in language. */
                            // i.e., void accept(Visitor visitor)

}
