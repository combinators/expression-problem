package expression.history;

import expression.DomainModel;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Stack;

/**
 * This class models a specific, linear evolution for the domain
 *
 * Goal is to be able to document the evolution that occurs as a linear expansion.
 * The topmost node reflects the most recent evolution, and a stack or prior evolutions are behind it.
 */
public class History implements Iterable<DomainModel> {

    /** Past nodes are stacked. */
    ArrayList<Node> chain = new ArrayList<>();

    /** Inner class to store structure of history. */
    class Node {
        final String tag;
        final DomainModel domain;

        Node(DomainModel d, String tag) {
            this.tag = tag;
            this.domain = d;
        }
    }

    /** The starting point for all evolutionary history using default base tag. */
    public History() { }

    public int size() { return chain.size(); }

    public Iterator<DomainModel> iterator() {
        if (chain.isEmpty()) { return new ArrayList<DomainModel>().iterator(); }

        return iterator(chain.get(0).tag);
    }

    /** Given a desired tag in our history, produce full reverse history to base.
     *
     * @param tag
     * @return
     */
    public Iterator<DomainModel> iterator(String tag) {
        ArrayList<DomainModel> result = new ArrayList<>();

        boolean extract = false;
        for (Node n : chain) {
            if (n.tag.equals(tag)) {
                extract = true;
            }

            if (extract) {
                result.add(n.domain);
            }
        }

        return result.iterator();
    }

    /**
     * Extend the given evolution history with a new Domain, as labeled by tag.
     *
     * Note that nodes are stored in reverse order; thus most recent node is first.
     *
     * @param tag
     * @param extend
     */
    public void extend (String tag, DomainModel extend) {
        Node n = new Node(extend, tag);
        chain.add(0, n);
    }

    /** Just get specific domain model. */
    public DomainModel get(String tag) {
        for (Node n : chain) {
            if (n.tag.equals(tag)) {
                return n.domain;
            }
        }

        return null;
    }

    /** Collapse everything into one from tag to beyond. */
    public DomainModel flatten(String tag) {
        DomainModel merged = null;
        boolean extract = false;

        for (Node n : chain) {

            if (n.tag.equals(tag)) {
                extract = true;
            }

            if (extract) {
                if (merged == null) {
                    merged = n.domain;
                } else {
                    merged = merged.merge(n.domain);
                }
            }
        }

        return merged;
    }

    /** Collapse everything into one. */
    public DomainModel flatten() {
        DomainModel merged = null;
        for (Node n : chain) {
            if (merged == null) {
                merged = n.domain;
            } else {
                merged = merged.merge(n.domain);
            }
        }

        return merged;
    }
}
