package expression.instances;

import expression.Exp;

import java.util.stream.Stream;

/**
 * Represents an actual instance
 */
public interface Instance {
    /** Return sub-instances. */
    Stream<Instance> subInstances();

    /** Return the domain model Exp subtype for this given instance. */
    Exp self();
}
