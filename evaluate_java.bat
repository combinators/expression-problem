@ECHO OFF

@REM 'java' and 'javac' must be present somewhere.
@REM Either you have already on your system path (option1)
@REM or you set to a valid JDK installation (preferably JDK 1.8)
@REM OPTION 1: set JAVAC=javac
@REM OPTION 2: set JAVAC=C:\Users\name\Desktop\jdk1.8.0_161\bin\javac.exe
@REM
@REM Change as necessary
set JAVAC=javac
set JAVA=java

@REM Refer back to the JAR files in top-level so they can be accessed
set CLASSPATH=..\..\..\..\..\dependencies\junit-4.11.jar;.;..\..\..\..\..\dependencies\org.hamcrest.core_1.3.0.v20180420-1519.jar

if exist target (
    rem file exists
) else (
    @echo "No target directory"
    goto:EOF
)

cd target

if exist ep-originalPrototype (
    rem file exists
) else (
    @echo "ep-originalPrototype has not yet been generated"
    cd ..
    goto:EOF
)

cd ep-originalPrototype

if exist java (
    rem file exists
) else (
    @echo "No Java EP approaches have been generated"
    cd ..\..
    goto:EOF
)

@REM All output will appear in the full-report.txt
@echo REPORT > java-report.txt

cd java

setlocal enabledelayedexpansion
for /D %%d in (.\*) do (

  @echo %%d >> ..\java-report.txt
  @echo %%d
  cd %%d
    for /D %%m in (.\*) do (
      @echo %%m >> ..\..\java-report.txt
      @echo %%m
      cd %%m
        echo "compiling %%d %%m"
        %JAVAC% %%d\*.java >> ..\..\..\java-report.txt 2>&1 && (
          echo "... success"

          for %%v in (%%d\TestSuite*.java) do (
            %JAVA% org.junit.runner.JUnitCore %%~nd.%%~nv >> ..\..\..\java-report.txt 2>&1 && (
                @echo "%%~nv success"
              ) || (
                @echo "%%~nv failed"
                @echo failed on %%~nd.%%~nv >> ..\..\..\java-report.txt
              )
          )
        ) || (
          echo "... FAIL"
        )
      cd ..
    )
  cd ..
)

echo "-------------"                >> ..\java-report.txt
echo "FINALLY DONE"                 >> ..\java-report.txt
echo "-------------"                >> ..\java-report.txt

@rem go back home
cd ..\..\..

dir target\ep-originalPrototype\java-report.txt
