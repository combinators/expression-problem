package expression.instances;

/**
 * Not yet clear if a full visitor is needed, but it's here in any case.
 */
public abstract class Visitor {

    /** Visit operation. */
    abstract void visit(Instance op);
}
