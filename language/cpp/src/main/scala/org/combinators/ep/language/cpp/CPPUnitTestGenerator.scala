package org.combinators.ep.language.cpp   /*DI:LD:AI*/

import org.combinators.ep.domain.{BaseDomain, ModelDomain}

/**
  * Each evolution has opportunity to enhance the code generators.
  *
  * This code conforms to JUnit Test cases
  */
trait CPPUnitTestGenerator extends TestGenerator with PerformanceTestGenerator with CPPGenerator {
  val domain: BaseDomain with ModelDomain

  /**
    * Represents the sequence of total test cases.
    */
  def testGenerator : Seq[UnitTest] = Seq.empty

  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  def generateSuite(pkg: Option[String], beforeCPPUnit:Seq[String]): Seq[CPPFile] = {
    val allTests = testGenerator.zipWithIndex.map{ case (t, num) =>

      new CPPStatement(
        s"""
           |TEST_GROUP(TestGroup$num)
           |{
           |};
           |
              |TEST(TestGroup$num, a$num)
           |{
           |   ${t.mkString("\n")}
           |}""".stripMargin
      )
    }

    // include performance timing code
    val sa = new StandAlone("test_e0",
      s"""
         |#include <sys/time.h>
         |long diffTimer (struct timeval *before, struct timeval *after) {
         |  long ds = after->tv_sec - before->tv_sec;
         |  long uds = after->tv_usec - before->tv_usec;
         |
         |  /* if secs are same, then return simple delta */
         |  if (ds == 0) {
         |    return uds;
         |  }
         |
         |  /* ok, so we are turned over. Something like: */
         |  /* before: 1179256745 744597                  */
         |  /* after:  1179256746 73514                   */
         |
         |  /* if we have 'turned over' then account properly */
         |  if (uds < 0) {
         |    ds = ds - 1;
         |    uds += 1000000;
         |  }
         |
         |  return 1000000*ds + uds;
         |}
         |
         |${allTests.mkString("\n")}
         |
         |int main(int ac, char** av)
         |{
         |  MemoryLeakWarningPlugin::turnOffNewDeleteOverloads();
         |  return CommandLineTestRunner::RunAllTests(ac, av);
         |}""".stripMargin.split("\n")
    )

    // for memory detection on CPP, some standard template imports (i.e., <map>) must be done FIRST, so do so here, if necessary
    sa.addHeader(beforeCPPUnit)

    sa.addHeader(Seq(
      """#include "CppUTest/TestHarness.h" """,
      """#include "CppUTest/SimpleString.h" """,
      """#include "CppUTest/PlatformSpecificFunctions.h" """,
      """#include "CppUTest/TestMemoryAllocator.h" """,
      """#include "CppUTest/MemoryLeakDetector.h" """,
      """#include "CppUTest/CommandLineTestRunner.h" """))

    Seq(sa)
  }
}