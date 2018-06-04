package example.expression.Straight

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.stmt.Statement
import example.expression.j.{TestGenerator, e4}
import org.combinators.templating.twirl.Java

trait E4_Generator extends StraightGenerator with TestGenerator with e4 {
  import domain._

}