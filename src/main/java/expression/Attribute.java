package expression;

import expression.types.TypeInformation;

/** An attribute is assumed to have get/set methods. */
public class Attribute extends Method {

    /** Attribute name. */
    public final String attName;

    /** Attribute type. */
    public final TypeInformation attType;

    public Attribute(String attName, TypeInformation attType) {
        super();
        this.attName = attName;
        this.attType = attType;
    }
}
