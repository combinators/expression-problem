@echo off

set PYTHON="c:\Program Files\Python311\python.exe"

set "dirPath=expression-problem"
dir "%dirPath%" >nul 2>&1
if %errorlevel% equ 0 (
    echo Updating existing directory
    cd expression-problem
    git pull
) else (
    echo Retrieving git directory
    git clone https://github.com/combinators/expression-problem.git
    cd expression-problem

    echo Switching to IL branch
    git checkout IL
)

echo Generating all Java solutions. This will take some time.
echo T0
date /T
time /T
call sbt "language-java/runMain org.combinators.ep.language.java.GenerateAll"
echo T1
date /T
time /T

cd target
for %%a in (ep-java ep-java-d1d2 ep-java-j ep-java-merging ep-java-extended ep-java-third-alternate) do (
  @echo off
  echo %%a
  cd %%a

  echo T2 %%a
  date /T
  time /T
  %PYTHON% ..\..\scripts\compile-java.py
  echo T3 %%a
  date /T
  time /T

  %PYTHON% ..\..\scripts\process-java.py > STATISTICS
  echo T4 %%a
  date /T
  time /T

  cd ..
)


echo Generating all Scala solutions. This will take some time.
echo T5
date /T
time /T
call sbt "language-newScala/runMain org.combinators.ep.language.scala.codegen.GenerateAll"
echo T6
date /T
time /T

cd target
for %%a in (ep-scala ep-scala-d1d2 ep-scala-j ep-scala-merging ep-scala-extended ep-scala-third-alternate) do (
  @echo off
  echo %%a
  cd %%a

  echo T7 %%a
  date /T
  time /T
  %PYTHON% ..\..\scripts\compile-scala.py
  echo T8 %%a
  date /T
  time /T

  %PYTHON% ..\..\scripts\process-scala.py > STATISTICS
  echo T9 %%a
  date /T
  time /T

  cd ..
)

echo DONE
echo T10
date /T
time /T
