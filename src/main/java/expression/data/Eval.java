package expression.data;

import expression.Operation;
import expression.types.Types;

public class Eval extends Operation {

    public Eval() {
        super("eval", Types.Double);
    }
}
