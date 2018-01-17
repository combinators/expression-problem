package expression;

import expression.types.TypeInformation;

public class FunctionMethod extends Method {

    /** Method name. */
    public final String name;

    /** For no return type, use PrimitiveTypes.Void. */
    public final TypeInformation returnType;

    public FunctionMethod(String name, TypeInformation retType) {
        super();
        this.name = name;
        this.returnType = retType;
    }
}
