package org.combinators.ep.util;

import java.util.Optional;
import java.util.Iterator;

public interface Tree {
    default java.util.Optional<Leaf<?>> asLeaf() { return java.util.Optional.empty(); }
    default java.util.Optional<Node> asNode() {	return java.util.Optional.empty(); }

    default boolean equals(org.combinators.ep.util.Tree o) {
        java.util.Optional<Boolean> leafCheck = this.asLeaf().flatMap(leaf -> o.asLeaf().map(leaf2 -> Boolean.valueOf(leaf.value.equals(leaf2.value))));
        java.util.Optional<Boolean> nodeCheck = this.asNode().flatMap(node -> o.asNode()
                .map(node2 -> {
                    if (!(node2.label == node.label)) { return false; }    // must be same label
                    if (node2.subtrees.size() != node.subtrees.size()) { return false; }  // short-circuit if not same length

                    java.util.Iterator<Tree> it1 = node.subtrees.iterator();   // all children must match.
                    java.util.Iterator<Tree> it2 = node2.subtrees.iterator();

                    while (it1.hasNext() && it2.hasNext()) {
                        if (!it1.next().equals(it2.next())) { return false; }
                    }

                    return true;
                }));

        // only two possibilities, else false
        return (leafCheck.orElse(nodeCheck.orElse(false)));
    }

    default boolean defaultEquals(Object o) {
        if (this == o) return true;
        if (o == null || !(Tree.class.isInstance(o))) return false;
        return equals((Tree)o);
    }
}