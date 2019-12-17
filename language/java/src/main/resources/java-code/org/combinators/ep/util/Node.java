package org.combinators.ep.util;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public final class Node implements Tree {
    public final int label;
    public final List<Tree> subtrees;

    @Override
    public int hashCode() {
        return Objects.hash(label, subtrees);
    }

    @Override
    public boolean equals(Object other) {
        return defaultEquals(other);
    }

    public Node(int label, Tree ... children) {
        this.label = label;
        this.subtrees = Arrays.asList(children);
    }

    @Override
    public java.util.Optional<Node> asNode() { return java.util.Optional.of(this); }

    @Override
    public String toString() {
        return "Node{" +
                "label=" + label +
                ", subtrees=" + subtrees.stream().map(Tree::toString).collect(Collectors.joining(",", "[", "]")) +
                '}';
    }
}