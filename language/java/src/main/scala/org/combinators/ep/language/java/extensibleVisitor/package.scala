package org.combinators.ep.language.java

import org.combinators.ep.domain.math.{I2, M0, M1, M3}

/**
  * This package contains the ExtensibleVisitor solution to EP, first described in
  * "Synthesizing OO and Functional Design to promote Reuse" by Shriram Krishnamurthi,
  * Matthias Felleisen, Daniel Friedman [[https://dl.acm.org/citation.cfm?id=679709]].
  *
  * == Approach ==
  *
  * The ExtensibleVisitor solution is based on the visitor design pattern but avoids the need
  * to modify the Visitor interface with each new data-type.
  *
  * Here is the fully generated solution for M2, demonstrating how to deal with adding data-types
  * and adding operations.
  *
  * There exists an abstract base class, Exp, that presents a standard ''accept'' method:
 **
 *{{{
*package visitor;
*public abstract class Exp {
    *public abstract <R> R accept(Visitor<R> v);
*}
*}}}
 *
 * Each data-type exists as a Java class that extends the Exp base class:
 **
 *{{{
*package visitor;
*public class Lit extends Exp {
    *private Double value;
 **
 *public Lit(Double value) { this.value = value; }
    *public Double getValue() { return this.value; }
    *public <R> R accept(Visitor<R> v) {
        *return v.visit(this);
    *}
*}
*}}}
 *
 * The '''Add''' data-type has the following implementation:
 **
 *{{{
*package visitor;
*public class Add extends Exp {
    *private Exp left;
    *private Exp right;
 **
 *public Add(Exp left, Exp right) {
        *this.left = left;
        *this.right = right;
    *}
 **
 *public Exp getLeft() { return this.left; }
    *public Exp getRight() { return this.right; }
 **
 *public <R> R accept(Visitor<R> v) {
        *return v.visit(this);
    *}
*}
*}}}
 *
 * Both '''Add''' and '''Lit''' accept a Visitor object, which contains the logic of a specific
  * operation as applied to these data-types.
  *
  * The '''Eval''' operation is part of the first [[M0]] evolution, and its definition
  * is as follows. Note that it provides ''visit'' implementations for the '''Lit''' and '''Add'''
  * data-types, which are also defined in [[M0]]. The ''makeEval'' operation returns an
  * instance of Eval on demand, and this method will be overridden by future extensions, thus
  * allowing unlimited extensions as new data-types are defined.
 **
 *{{{
*package visitor;
*public class Eval implements Visitor<Double> {
    *public Double visit(Lit e) {
        *return e.getValue();
    *}
 **
 *public Double visit(Add e) {
        *return e.getLeft().accept(makeEval()) + e.getRight().accept(makeEval());
    *}
 **
 *Eval makeEval() {
        *return new Eval();
    *}
*}
*}}}
 *
 * In [[M1]] the '''Sub''' data-type is defined:
 **
 *{{{
*package visitor;
*public class Sub extends Exp {
    *private Exp left;
    *private Exp right;
 **
 *public Sub(Exp left, Exp right) {
        *this.left = left;
        *this.right = right;
    *}
 **
 *public Exp getLeft() { return this.left; }
    *public Exp getRight() { return this.right; }
 **
 *public <R> R accept(Visitor<R> v) {
        *if (v instanceof VisitorSub) {
            *return ((VisitorSub<R>) v).visit(this);
        *}
        *throw new RuntimeException("Older visitor used with newer datatype variant.");
    *}
*}
*}}}
 *
 * This implementation is nearly identical to '''Add''' but observe how there is a
  * dynamic cast of the visitor in its ''accept'' method to ensure that only proper visitors
  * capable of visiting the '''Sub''' data-type are allowed. A new interface that extends
  * '''Visitor''' is defined.
 **
 *{{{
*package visitor;
*public interface VisitorSub<R> extends Visitor<R> {
    *public R visit(Sub exp);
*}
*}}}
 *
 * Thus a '''VisitorSub''' visitor provides logic for visiting '''Lit''', '''Add''', and '''Sub'''
  * data-types. Instead of modifying the previously created visitor, a new visitor implementation
  * is provided for all existing operations, such as '''EvalSub''':
 **
 *{{{
*public class EvalSub extends Eval implements VisitorSub<Double> {
    *public Double visit(Sub e) {
        *return e.getLeft().accept(makeEval()) - e.getRight().accept(makeEval());
    *}
 **
 *Eval makeEval() {
        *return new EvalSub();
    *}
*}
*}}}
 *
 * Note how '''EvalSub''' extends the previous class, '''Eval''' to provide an implementation
  * that describes how to evaluate the '''Sub''' data-type. The ''makeEval'' method is overridden
  * to return the latest instance of '''Eval'''.
  *
  * == Test cases ==
  *
  * JUnit test cases are generated to validate this implementation. Each test case provides
  * default implementations for the latest implementation of all known operations
  * (i.e., ''makeEval''). Various instances are constructed, with invocations to these visitors.
 **
 *{{{
*package visitor;
*import junit.framework.TestCase;
*public class TestSuite0 extends TestCase {
    *public void test() {
        *assertEquals(3.0, new Add(new Lit(1.0), new Lit(2.0)).accept(makeEval()));
        *assertEquals(5.0, new Lit(5.0).accept(makeEval()));
    *}
 **
 *Prettyp makePrettyp() { return new Prettyp(); }
    *Eval makeEval() { return new EvalSub(); }
    *Idz makeIdz() { return new IdzSub(); }
*}
*}}}
 *
 * == Evolutions ==
  *
  * There are six fundamental evolutions completed:
  * [[M0]],
  * [[M1]],
  * [[M2]],
  * [[M3]],
  * [[M4]],
  * [[M5]],
  * [[M6]],
  *
  * There is one composite branch [[C1]],
  * that connects together the evolutions
  * up to [[M3]] and the evolutions up to [[I2]], thus demonstrating the
  * ability to join together previously independent paths.
  */
package object extensibleVisitor {

}
