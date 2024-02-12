@ECHO OFF

@REM Assume that 'sbt' is already installed someplace

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

if exist scala (
    rem file exists
) else (
    @echo "No Scala EP approaches have been generated"
    cd ..\..
    goto:EOF
)

@REM All output will appear in the scala-report.txt
@echo REPORT > scala-report.txt

cd scala

setlocal enabledelayedexpansion
for /D %%d in (.\*) do (
  @echo %%d >> ..\scala-report.txt
  @echo %%d
  cd %%d
    for /D %%m in (.\*) do (
      @echo %%m >> ..\..\scala-report.txt
      @echo %%m
      cd %%m
        cd
        echo "compiling %%d %%m"
        echo libraryDependencies += "org.scalatest" %%%% "scalatest" %% "3.2.18" %% "test" > build.sbt
        echo exit > exit

        if exist project (
            rem file exists
        ) else (
            mkdir project
        )
        if exist src (
            rem file exists
        ) else (
           mkdir src
           mkdir src\main
           mkdir src\main\scala
           mkdir src\test
           mkdir src\test\scala

           @REM move files (though of course this can only be done once)
           echo "moving files..."
           move *.scala src\main\scala >nul
           move src\main\scala\TestSuite*.scala src\test\scala >nul
        )

        echo "compiling and testing..."
        call sbt test >> ..\..\..\scala-report.txt 2>&1 <exit
        cd
        del exit
      cd ..
    )
  cd ..
)

echo "-------------"                >> ..\scala-report.txt
echo "FINALLY DONE"                 >> ..\scala-report.txt
echo "-------------"                >> ..\scala-report.txt

@rem go back home
cd ..\..\..

dir target\ep-firstVersion\scala-report.txt
