@echo off
@REM ASSUMES GenerateAll has already been run, with output in ep-scala-xxx

@REM directory in target/ where generated code is placed...
set DIR=%3

@REM expects to be run in primary directory
@REM generated into target\%DIR%
cd target
cd %DIR%

@REM No test cases yet. Just compile
cd %1
cd %2

echo ====================================== >> ..\..\jacoco.%1
echo %2-Compile-Begin                       >> ..\..\jacoco.%1
java -cp ..\..\..\..\scripts Time           >> ..\..\jacoco.%1
echo ====================================== >> ..\..\jacoco.%1

call sbt compile       >> ..\..\jacoco.%1

@REM go back to start
cd ..\..\..\..
