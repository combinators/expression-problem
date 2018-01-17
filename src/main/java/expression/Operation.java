package expression;

import expression.types.TypeInformation;

/**
 * This is the high-level representation of a desired operation.
 */
public abstract class Operation {

    /** Name of operation. */
    public final String name;

    /** Resulting type of operation. */
    public final TypeInformation type;

    public Operation(String name, TypeInformation type) {
        this.name = name;
        this.type = type;
    }

    /**
     * Two Operation objects are the same if they derive from same class.
     * Necessary for merging DomainModel.
     *
     * @param o   Other Operation subtype to check for equality.
     */
    @Override
    public boolean equals(Object o) {
        if (o == null) { return false; }
        if (o instanceof Operation) {
            return o.getClass() == getClass();
        }

        return false;
    }

    /**
     * Ensures hashing works.
     *
     * @return
     */
    public int hashCode() {
        return getClass().hashCode();
    }
}
