package expression.operations;

import expression.extensions.SimplifyBase;

/**
 * Simplifies (x+0) -> x and (0+x) -> x and (0+0) -> 0
 */
public class SimplifyAdd extends SimplifyBase {
    public SimplifyAdd() {
        super();
    }
}
