package org.combinators.ep.language.inbetween.any /*DI:LI:AI*/

import org.combinators.cogen.{ AbstractSyntax => AS }

class AbstractSyntax[FT <: FinalTypes] extends AS {
  type CompilationUnit = org.combinators.ep.language.inbetween.any.CompilationUnit[FT]
  type Import = org.combinators.ep.language.inbetween.any.Import[FT]
  type Expression = org.combinators.ep.language.inbetween.any.Expression[FT]
  type Type = org.combinators.ep.language.inbetween.any.Type[FT]
  type Statement = org.combinators.ep.language.inbetween.any.Statement[FT]
  type UnitTest = Unit //TODO: org.combinators.ep.language.inbetween.any.UnitTest[FT]
  type Name = org.combinators.ep.language.inbetween.any.Name[FT]
}
