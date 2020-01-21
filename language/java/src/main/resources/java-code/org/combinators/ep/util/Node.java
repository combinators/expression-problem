package org.combinators.ep.util;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public final class Node implements org.combinators.ep.util.Tree {
    public final int label;
    public final java.util.List<Tree> subtrees;

    @Override
    public int hashCode() {
        return java.util.Objects.hash(label, subtrees);
    }

    @Override
    public boolean equals(Object other) {
        return defaultEquals(other);
    }

    public Node(int label, org.combinators.ep.util.Tree ... children) {
        this.label = label;
        this.subtrees = java.util.Arrays.asList(children);
    }

    @Override
    public java.util.Optional<Node> asNode() { return java.util.Optional.of(this); }

    @Override
    public String toString() {
        return "Node{" +
                "label=" + label +
                ", subtrees=" + subtrees.stream().map(Tree::toString).collect(java.util.stream.Collectors.joining(",", "[", "]")) +
                '}';
    }
}
