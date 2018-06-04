package example.expression.Straight

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.stmt.Statement
import example.expression.j.{TestGenerator, e3}
import org.combinators.templating.twirl.Java

trait E3_Generator extends StraightGenerator with TestGenerator with e3 {
  import domain._

}