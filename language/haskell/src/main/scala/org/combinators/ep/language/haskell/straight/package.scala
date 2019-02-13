package ep.haskell

/**
  * This package contains the strawman Haskell implementation.
  *
  * Technically, this is not a valid solution, since one must change existing classes when new
  * operations are defined. Still, it is provided here to demonstrate the mechanics of generating
  * code solutions for EP.
  *
  * == Approach ==
  *
  * The straight Haskell approach is based on the idea that you construct a class for
  * every data-type, and each of the requested operations becomes a method of each class.
  *
  * Here is the fully generated solution for M0:
  *
  * We start with a DataTypes module that defines the structure of all data-types.
  *
  * {{{
       module DataTypes where

       -- All types are classified as data
       data Exp = Lit Double  | Add Exp Exp
  * }}}
  *
  * Each operation is defined in its own module, defining implementations for each
  * of the existing data types.
  *
  * {{{
       module Eval where
       import DataTypes

       eval :: Exp  -> Double
       eval (Lit value)  = value
       eval (Add left right)  = (eval (left)) + (eval (right))
  * }}}
  *
  * == Test cases ==
  *
  * Hunit test cases are generated to validate this implementation:
  *
{{{
       module Main where
       import Test.HUnit
       import DataTypes

       import Eval
       import Idz
       test_v0 = TestCase (assertEqual "EqualsTestCase" (3.0) (eval (Add (Lit 1.0) (Lit 2.0) ) ))
       test_v1 = TestCase (assertEqual "EqualsTestCase" (5.0) (eval (Lit 5.0) ))
       test_all = TestList [ TestLabel "0" test_v0,TestLabel "1" test_v1 ]

       main :: IO Counts
       main  = runTestTT test_all
}}}
  *
  */
package object straight {

}