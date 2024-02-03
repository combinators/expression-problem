# To access old version

git clone this EP repository (https://github.com/combinators/expression-problem) and then switch to the `archived` branch.

`git checkout archived`

You will then need to modify the `build.sbt` file because the Scala Version is so out of date. The following works.

`scalaVersion := "2.12.17"`

Launch `sbt` and then you can generate code solutions in different languages.

## Java

In sbt, execute the command `language-java/run`.

Now in a separate terminal window request access to a git repository that would be generated, something like:

`git clone http://localhost:9000/algebra/m5/m5.git`

The pattern of all of these requests is:

`git clone http://localhost:9000/{approach}/{stage}/{stage}.git`

Valid values include:

 * stage: m0, m1, m2, m3, m4, m5, m6, m7, m8, i1, i2, p1, s0, s1, c1
 * approach: algebra, extensibleVisitor, interpreter, oo, trivially, visitor

## Haskell

In sbt, execute the command `language-haskell/run`.

Now in a separate terminal window request access to a git repository that would be generated, something like:

`git clone http://localhost:9000/grow/m5/m5.git`


Valid values include:

* stage: m0, m1, m2, m3, m4, m5, m6
* approach: alacarte, grow, straight

## C++

In sbt, execute the command `language-cpp/run`.

Now in a separate terminal window request access to a git repository that would be generated, something like:

`git clone http://localhost:9000/oo/m5/m5.git`

Valid values include:

* stage: m0, m1, m2, m3, m4, m5, m6
* approach: oo, visitor, visitorTable

## GJ

In sbt, execute the command `language-gj/run`.

Now in a separate terminal window request access to a git repository that would be generated, something like:

`git clone http://localhost:9000/wadler/m1/m1.git`

Valid values include:

* stage: m0, m1
* approach: wadler

## Scala

In sbt, execute the command `language-scala/run`.

Now in a separate terminal window request access to a git repository that would be generated, something like:

`git clone http://localhost:9000/oo/m5/m5.git`

Valid values include:

* stage: m0, m1, m2, m3, m4, m5, m6
* approach: oo, functional, straight


