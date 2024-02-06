package org.combinators.ep.language.scala     /*DI:LD:AD*/

import org.combinators.ep.domain.math._

/**
  * This package contains the strawman functional solution to the Expression Problem in Haskell.
  *
  * Technically, this is not a valid solution, since one must change existing code when new
  * operations are defined. Still, it is provided here to demonstrate the mechanics of generating
  * code solutions for EP.
  *
  * == Approach ==
  *
  * The straight Functional approach (in Haskell) is based on the idea that you construct a
  * common data structure based on alternatives.
  *
  * Here is the fully generated solution for M0:
  *
  *
*{{{
*module DataTypes where
 **
 *-- All types are classified as data
*data Exp = Lit Double  | Add Exp Exp
*}}}
 *
 * Each operation would exist as a module that defines functions that apply to the existing
  * data structures.
  *
*{{{
*module Eval where
*import DataTypes
 **
 *eval :: Exp  -> Double
*eval (Lit value) = value
*eval (Add left right)  = (eval (left)) + (eval (right))
*}}}
 *
 * There are two cases for eval, covering '''Lit''' and '''Add'''.
  *
  *
  * This implementation is not strictly a solution to EP because one has to change existing
  * cases whenever new data types are added to the system
  *
  * == Test cases ==
  *
  * HUnit test cases are generated to validate this implementation:
  *
*{{{
*module Main where
*import Test.HUnit
*import DataTypes
 **
 *import Eval
*import Idz
*test_v0 = TestCase (assertEqual "EqualsTestCase" (3.0) (eval (Add (Lit 1.0) (Lit 2.0) ) ))
*test_v1 = TestCase (assertEqual "EqualsTestCase" (5.0) (eval (Lit 5.0) ))
*test_all = TestList [ TestLabel "0" test_v0,TestLabel "1" test_v1 ]
 **
 *main :: IO Counts
*main  = runTestTT test_all
*}}}
 *
 *
  * == Evolutions ==
  *
  * There are six fundamental evolutions completed: [[M0]], [[M1]],
  * [[M2]], [[M3]],[[M4]], [[M5]],[[M6]]
  */
package object functional {

}