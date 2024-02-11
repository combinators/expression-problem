@ECHO OFF

@REM you Need to copy this file into target\java so it will properly run

@REM must be present somewhere. Either you have already on your system path (option1)
@REM or you set to a valid JDK installation (preferably JDK 1.8)
@REM OPTION 1: set JAVAC=javac
@REM OPTION 2: set JAVAC=C:\Users\name\Desktop\jdk1.8.0_161\bin\javac.exe
set JAVAC=javac
set JAVA=java

@REM Refer back to the JAR files in top-level so they can be accessed
set CLASSPATH=..\..\..\..\..\junit-4.11.jar;.;..\..\..\..\..\org.hamcrest.core_1.3.0.v20180420-1519.jar

@REM All output will appear in the full-report.txt
@echo REPORT > full-report.txt

if exist target (
    rem file exists
) else (
    @echo "No target directory"
    goto:EOF
)

cd target

if exist ep-firstVersion (
    rem file exists
) else (
    @echo "ep-firstVersion has not yet been generated"
    cd ..
    goto:EOF
)

cd ep-firstVersion

if exist java (
    rem file exists
) else (
    @echo "No Java EP approaches have been generated"
    cd ..\..
    goto:EOF
)

cd java

setlocal enabledelayedexpansion
for /D %%d in (.\*) do (

  @echo %%d >> full-report.txt
  @echo %%d
  cd %%d
    for /D %%m in (.\*) do (
      @echo %%m >> ..\full-report.txt
      @echo %%m
      cd %%m
        echo "compiling %%d %%m"
        %JAVAC% %%d\*.java >> ..\..\full-report.txt 2>&1 && (
          echo "... success"

          for %%v in (%%d\TestSuite*.java) do (
            %JAVA% org.junit.runner.JUnitCore %%~nd.%%~nv >> ..\..\full-report.txt 2>&1 && (
                @echo "%%~nv success"
              ) || (
                @echo "%%~nv failed"
                @echo failed on %%~nd.%%~nv >> ..\..\full-report.txt
              )
          )
        ) || (
          echo "... FAIL"
        )
      cd ..
    )
  cd ..
)

echo "-------------"                >> full-report.txt
echo "FINALLY DONE"                 >> full-report.txt
echo "-------------"                >> full-report.txt

@rem go back home
cd ..\..\..

dir target\ep-firstVersion\java\full-report.txt
