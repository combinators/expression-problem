#!/bin/bash

# 'java' and 'javac' must be present on path, or set here
export JAVAC=javac
export JAVA=java

# Refer back to the JAR files in top-level so they can be accessed
export CLASSPATH=../../../../../dependencies/junit-4.11.jar:.:../../../../../dependencies/org.hamcrest.core_1.3.0.v20180420-1519.jar

if [ ! -d target ]
then
  echo "No target directory"
  exit
fi

cd target

if [ ! -d ep-firstVersion ]
then
  echo "ep-firstVersion has not yet been generated"
  exit
fi

cd ep-firstVersion

if [ ! -d java ]
then
  echo "No Java EP approaches have been generated"
  exit
fi

# All output will appear in the java-report.txt
echo REPORT > java-report.txt

cd java

for d in `ls`
do
  echo $d >> ../java-report.txt
  echo $d
  cd $d
    for m in `ls`
    do
      echo $m >> ../../java-report.txt
      cd $m
         echo "compiling $d $m"
         echo "compiling $d $m" >> ../../../java-report.txt 2>&1
         $JAVAC $d/*.java >> ../../../java-report.txt 2>&1
         if [ $? -eq 0 ]
         then
           echo "success on compile $d.$m"
           for tc in `ls $d/TestSuite*.java`
           do
             bc=`basename $tc`
             $JAVA org.junit.runner.JUnitCore $d.${bc%.*} >> ../../../java-report.txt 2>&1
             if [ $? -eq 0 ]
             then
               echo $d.${tc%.*} successful
               echo $d.${tc%.*} successful >> ../../../java-report.txt
             else
               echo ${tc%.*} failed
               echo $d.${tc%.*} failed >> ../../../java-report.txt
             fi
           done
         else
           echo "failure on compile $d.$m"
           echo "failure on compile $d.$m" >> ../../../java-report.txt
         fi
      cd ..
    done
  cd ..
done

echo "-------------"                >> ../java-report.txt
echo "FINALLY DONE"                 >> ../java-report.txt
echo "-------------"                >> ../java-report.txt

ls -l ../java-report.txt
