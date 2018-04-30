package expression.operations;

import expression.extensions.SimplifyBase;

/**
 * Operation that simplifies expression based on known patterns from the underlying
 * literals and expression subtypes.
 *
 * As new operations are defined, additional opportunities exist for simplification, and these
 * would be registered in the domain model. For example, once Sqrt and Power are defined,
 * then ( Sqrt ( Power 5 2 ) ) == Lit(5)
 *
 * Note that symbolic simplification could be more complicated, such as, (a-b)*(a+b) <--> (a^2 - b^2)
 * but that would be more for symbolic execution.
 */
public class SimplifyExpr extends SimplifyBase {
    public SimplifyExpr() {
        super();
    }
}
