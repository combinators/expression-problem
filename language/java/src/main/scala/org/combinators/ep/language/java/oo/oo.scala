package org.combinators.ep.language.java     /*DI:LD:AD*/

import org.combinators.ep.domain.math._

/**
  * This package contains the Strawman object-oriented solution to the Expression Problem in Java.
  *
  * Technically, this is not a valid solution, since one must change existing classes when new
  * operations are defined. Still, it is provided here to demonstrate the mechanics of generating
  * code solutions for EP.
  *
  * == Approach ==
  *
  * The straight OO approach (in Java) is based on the idea that you construct a class for
  * every data-type, and each of the requested operations becomes a method of each class.
  *
  * Here is the fully generated solution for M0:
  *
  * There exists an abstract base class, Exp, that defines the collection of available operations:
  *
*{{{
*package oo;
*  public abstract class Exp {
*  public abstract Double eval();
*}
*}}}
 *
 * Each data-type would exist as a Java class that extends the Exp base class:
 **
 *{{{
    *package oo;
    *public class Lit extends Exp {
    *  private Double value;
    *
    *  public Lit(Double value) { this.value = value; }
    *  public Double eval() { return value; }
    *}
    *}}}
 *
 * The Add data-type has the following implementation:
 **
 *{{{
*package oo;
*public class Add extends Exp {
*  private Exp left;
*  private Exp right;
*
*  public Add(Exp left, Exp right) {
*    this.left = left;
*    this.right = right;
*  }
*
*  public Double eval() { return left.eval() + right.eval(); }
*}
*}}}
 *
 * This implementation is not strictly a solution to EP because one has to change existing
  * classes whenever the system must evolve to add a new operation.
  *
  * == Test cases ==
  *
  * JUnit test cases are generated to validate this implementation:
  *
*{{{
*public void test() {
*  assertEquals(3.0, new Add(new Lit(1.0), new Lit(2.0)).eval());
*  assertEquals(5.0, new Lit(5.0).eval());
*}
*}}}
 *
 * == Evolutions ==
  * There are six fundamental evolutions completed: [[M0]], [[M1]], [[M2]], [[M3]], [[M4]], [[M5]], [[M6]],
  *
  * There is an independent branch [[I2]], that extends from [[M1]],
  * thus demonstrating the ability to fork independent evolutionary paths.
  *
  * There is one composite branch [[MC1]] that connects together the evolutions
  * up to [[M3]] and the evolutions up to [[I2]], thus demonstrating the
  * ability to join together previously independent paths.
  */
package object oo {

}