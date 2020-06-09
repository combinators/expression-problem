package org.combinators.jgitserv    /*DI:LI:AI*/

import java.nio.file.Path
import java.nio.file.Paths

import org.combinators.templating.persistable.Persistable

object TestData {
  implicit val persistString: Persistable.Aux[(String, String)] =
    new Persistable {
      type T = (String, String)
      def rawText(elem: (String, String)): Array[Byte] = elem._1.getBytes
      def path(elem: (String, String)): Path = Paths.get(elem._2)
    }
}
import TestData._

object Test extends GitService(
  Seq(
    BranchTransaction.empty("TestBranch")
      .persist( "Test1" -> "test1.txt")
      .persist ("Test2" -> "test2.txt")
      .commit("FirstTest")
      .fork("OtherBranch")
      .persist("Test3" -> "test3.txt")
      .commit("SecondTest"),
    BranchTransaction
      .checkout("TestBranch")
      .persist("Test4" -> "test4.txt")
      .commit("ThirdTest")
  ))
