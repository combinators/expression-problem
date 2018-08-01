package example.expression.j

import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.generator.BinaryMethodBase

/**
  * Stamdard way to deal with atree
  *
  *
   public Tree visit(Mult e) {
        return new Node(java.util.Arrays.asList(left.astree(), right.astree()), DefinedSubtypes.Mult);
    }

  public Tree astree() {
        return new Node(java.util.Arrays.asList(left.astree(), right.astree()), DefinedSubtypes.Add);
    }

  */
trait StandardJavaMethodBase extends BinaryMethodBase {
  val domain:BaseDomain with ModelDomain


}
