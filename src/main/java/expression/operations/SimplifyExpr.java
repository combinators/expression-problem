package expression.operations;

import expression.extensions.SimplifyBase;

/**
 * Simplifies (x+0) -> x and (0+x) -> x and (0+0) -> 0
 * Also simplifies MULT
 *
 * TODO: Rename at some point
 */
public class SimplifyExpr extends SimplifyBase {
    public SimplifyExpr() {
        super();
    }
}
