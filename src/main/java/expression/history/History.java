package expression.history;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Optional;

/**
 * This class models a specific, linear evolution for the domain
 *
 * Goal is to be able to document the evolution path chosen.
 */
public class History<Domain> {
    /** Well-known tag for base. */
    public static final String BASE = "_BASE_";

    /** Find any tag within hierarchy. */
    HashMap<String, Node> map = new HashMap<>();

    /** Root tag. */
    String rootTag;

    /** Inner class to store structure of history. */
    class Node {
        final String tag;
        final Domain info;
        Node parent;

        HashMap<String, Node> children = new HashMap<>();

        Node(Domain d, String tag) {
            this.tag = tag;
            this.info = d;
            parent = null;
        }

        /** Extend node with unique tag; return false if already exists. */
        boolean extend(Node n) {
            if (children.containsKey(n.tag)) { return false; }
            children.put(n.tag, n);
            n.parent = this;
            return true;
        }

        Optional<Node> parent() {
            if (parent == null) { return Optional.empty(); }
            return Optional.of(parent);
        }
    }

    /** The starting point for all evolutionary history using default base tag. */
    public History(Domain base) {
        this (base, BASE);
    }

    /** The starting point for all evolutionary history using given tag. */
    public History (Domain base, String baseTag) {
        rootTag = baseTag;
        map.put(baseTag, new Node (base, baseTag));
    }

    /**
     * Given a desired tag in our history, produce full reverse history to base.
     *
     * @param tag
     * @return
     */
    public HistoryIterator iterator(String tag) {
        return new HistoryIterator(map.get(tag));
    }

    /**
     * Extend the given evolution history with a new Domain, as labeled by tag.
     *
     * Return false if pastTag doesn't exist.
     *
     * @param pastTag
     * @param extend
     * @param tag
     */
    public boolean extend (String pastTag, Domain extend, String tag) {
        Node parent = map.get(pastTag);
        if (parent == null) { return false; }

        Node n = new Node(extend, tag);
        if (parent.extend(n)) {
            map.put(tag, n);
            return true;
        }

        // can't extend, since tag already exists...
        return false;
    }

    /** Generate reverse history for a Node. */
    class HistoryIterator implements Iterator<Domain> {

        Node node;

        HistoryIterator(Node n) {
            node = n;
        }

        @Override
        public boolean hasNext() {
            return node != null;
        }

        @Override
        public Domain next() {
            Domain d = node.info;
            node = node.parent;
            return d;
        }
    }

    /** Retrieve specified DomainModel by history tag. */
    public Domain retrieve(String tag) {
        Node found = map.get(tag);

        if (found == null) { return null; }

        return found.info;
    }
}
