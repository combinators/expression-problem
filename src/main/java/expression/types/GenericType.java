package expression.types;

/**
 * Given an existing Type, make it generic.
 */
public class GenericType implements TypeInformation {

    public final TypeInformation base;
    public final TypeInformation generic;

    public GenericType(TypeInformation tpe, TypeInformation generic) {
        this.base = tpe;
        this.generic = generic;
    }
}
