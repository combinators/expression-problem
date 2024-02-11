@ECHO OFF

@REM you Need to copy this file into target\java so it will properly run

@REM must be present somewhere. Either you have already on your system path (option1)
@REM or you set to a valid JDK installation (preferably JDK 1.8)
@REM OPTION 1: set JAVAC=javac
@REM OPTION 2: set JAVAC=C:\Users\name\Desktop\jdk1.8.0_161\bin\javac.exe

@REM Make sure a valid junit is in this directory as well so it
@REM can be accessed two levels down. Copy the junit-4.11.jar file to be
@REM at the same location (target\java) as the execute.bat file
set CLASSPATH=..\..\junit-4.11.jar;.

@REM All output will appear in the full-report.txt
@echo REPORT > full-report.txt


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
        ) || (
          echo "... FAIL"
        )
      cd ..
    )
  cd ..
)

echo "-------------"                >> full-report.txt
echo "NOW REMOVING ALL TEST CASES"  >> full-report.txt
echo "NOW REMOVING ALL TEST CASES"
echo "-------------"                >> full-report.txt

for /D %%d in (.\*) do (

  @echo %%d >> full-report.txt
  @echo %%d
  cd %%d
    for /D %%m in (.\*) do (
      @echo %%m >> ..\full-report.txt
      @echo %%m
      cd %%m
        echo "compiling %%d %%m"
        del %%d\TestSuite*.java  2>nul
        del %%d\TestSuite*.class 2>nul
        %JAVAC% %%d\*.java >> ..\..\full-report.txt 2>&1 && (
          echo "... success"
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
