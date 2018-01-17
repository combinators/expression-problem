package expression.extensions;

import expression.Operation;
import expression.types.Types;

public class PrettyP extends Operation {

    public PrettyP() {
        super("print", Types.String);
    }
}
