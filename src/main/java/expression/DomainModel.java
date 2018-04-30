package expression;

import java.util.*;


/**
 * Represents the desired features.
 *
 * Note that 'Exp' is not in the domain of data types, only sub-types are.
 *
 * Any DomainModel can optionally refer back to a parent domain model
 */
public class DomainModel {

    /** Desired data types. */
    public List<Exp> data = new ArrayList<>();

    /** Desired operations. */
    public List<Operation> ops = new ArrayList<>();

    /** Parent. */
    final DomainModel parent;

    /**
     * Create empty domain model, suitable to be a parent base.
     */
    public DomainModel() {
        this(null, new ArrayList<Exp>(), new ArrayList<Operation>());
    }

    /** Return the parent (if it exists). */
    public Optional<DomainModel> getParent() {
        if (parent != null ) {
            return Optional.of(parent);
        }

        return Optional.empty();
    }

    /**
     * Construct initial Domain model from list of subtypes and operations.
     *
     * @param data    desired subtypes
     * @param ops     desired operations
     */
    public DomainModel(Collection<Exp> data, Collection<Operation> ops) {
        this (null, data, ops);
    }

    /**
     * Construct a new domain model which extends the provided parent.
     *
     * @param parent
     * @param data
     * @param ops
     */
    public DomainModel (DomainModel parent, Collection<Exp> data, Collection<Operation> ops) {
        this.parent = parent;
        this.data.addAll(data);
        this.ops.addAll(ops);
    }

    /**
     * Flatten hierarchy (if exists) to create a new domain model, with merged data and operations.
     *
     * @return      Returns a new DomainModel object merging data and operations.
     */
    public DomainModel flatten() {
        if (parent == null) { return this; }

        Set<Operation> mergedOps = new HashSet<>();
        Set<Exp> mergedData = new HashSet<>();

        // traverse backwards until done, and flatten all data and ops uniquely
        DomainModel next = this;
        while (next != null) {
            for (Exp e : next.data) {
                if (!mergedData.contains(e)) { mergedData.add(e); }
            }

            for (Operation o: next.ops) {
                if (!mergedOps.contains(o)) { mergedOps.add(o); }
            }

            next = next.parent;
        }

        return new DomainModel(mergedData, mergedOps);
    }

    /**
     * Accept a visitor for all model elements, recursively.

     * @param v     Visitor for the domain model
     */
    public void accept(Visitor v) {

        // To accept a set, we must visit each member of the set and be accepted on each element.
        for (Exp e : data) {
            e.accept (v);
        }

        for (Operation op: ops) {
            op.accept(v);
        }

        // Finally recognize that we have visited the set
        if (parent != null) {
            v.visit(this);
        }
    }

}
