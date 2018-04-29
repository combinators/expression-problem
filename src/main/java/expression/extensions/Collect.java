package expression.extensions;

import expression.Operation;
import expression.types.FrameworkTypes;
import expression.types.GenericType;
import expression.types.Types;

public class Collect extends Operation {

    public Collect() {
        super("collectList", new GenericType (FrameworkTypes.List, Types.Double));
    }
}
