@echo off
@REM ASSUMES GenerateAll has already been run, with output in ep-java-xxx

@REM directory in target/ where generated code is placed...
set DIR=%3

@REM expects to be run in primary directory
@REM generated into target\%DIR%
cd target
cd %DIR%

cd %1
cd %2

@REM The first compiles the code. The second compiles the test cases. There
@REM doesn't seem to be any way to avoid instrumentation in the second pass, but
@REM at least this doesn't conflate initial compilation time
echo ====================================== >> ..\..\jacoco.%1
echo %2-Compile-Begin                       >> ..\..\jacoco.%1
java -cp ..\..\..\..\scripts Time           >> ..\..\jacoco.%1
echo ====================================== >> ..\..\jacoco.%1
call sbt compile        >> ..\..\jacoco.%1

echo ====================================== >> ..\..\jacoco.%1
echo %2-Test-Begin                          >> ..\..\jacoco.%1
java -cp ..\..\..\..\scripts Time           >> ..\..\jacoco.%1
echo ====================================== >> ..\..\jacoco.%1
call sbt jacoco       >> ..\..\jacoco.%1

echo ====================================== >> ..\..\jacoco.%1
echo %2-Test-End                            >> ..\..\jacoco.%1
java -cp ..\..\..\..\scripts Time           >> ..\..\jacoco.%1
echo ====================================== >> ..\..\jacoco.%1

@REM go back to start
cd ..\..\..\..
