package expression;

/**
 * Not yet clear if a full visitor is needed, but it's here in any case.
 */
public abstract class Visitor{
    /** Visit a domain model. */
    abstract void visit(DomainModel dm);

    /** Visit exp. */
    abstract void visit(Exp exp);

    /** Visit operation. */
    abstract void visit(Operation op);
}
