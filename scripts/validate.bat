@echo off
@REM execute this script within PowerShell in the 'expression-problem' directory retrieved via git

set PYTHON="c:\Program Files\Python311\python.exe"

echo T0
date /T
time /T
call sbt "language-java/runMain org.combinators.ep.language.java.QuickValidation"
echo T1
date /T
time /T

@REM within target directory now
cd target
for %%a in (ep-java-quick) do (
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

@REM go back to expression-problem
cd ..

echo Generating all Scala solutions. This will take some time.
echo T5
date /T
time /T
call sbt "language-newScala/runMain org.combinators.ep.language.scala.codegen.QuickValidation"
echo T6
date /T
time /T

@REM now go back to target
cd target
for %%a in (ep-scala-quick) do (
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

@echo Confirming result
$SELJ = Select-String -Path ep-java-quick\STATISTICS -Pattern "error"
$SELS = Select-String -Path ep-scala-quick\STATISTICS -Pattern "error"

if ($SELJ -ne $null)
{
    @echo Error with Java Code Generation
    exit -1
}

if ($SELS -ne $null)
{
    @echo Error with Scala Code Generation
    exit -1
}
