#!/bin/bash

# A working g++ installation is required (tested with 7.5.0)
export CPPFLAGS=-std=c++11
export LIBS="-lCppUTest -lCppUTestExt"
export CPP=g++

# Refer back to the CPPUnit files in top-level so they can be accessed
export CPPUTEST=../../../../../dependencies/cpputest

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

if [ ! -d cpp ]
then
  echo "No CPP EP approaches have been generated"
  exit
fi

# All output will appear in the cpp-report.txt
echo REPORT > cpp-report.txt

cd cpp

for d in `ls`
do
  echo $d >> ../cpp-report.txt
  echo $d
  cd $d
    for m in `ls`
    do
      echo $m >> ../../cpp-report.txt
      cd $m
         echo "compiling $d $m"
         echo "compiling $d $m" >> ../../../cpp-report.txt 2>&1
         $CPP *.cpp  -I ${CPPUTEST}/include -L ${CPPUTEST}/cpputest_build/lib $LIBS $CPPFLAGS -o test >> ../../../cpp-report.txt 2>&1
         if [ $? -eq 0 ]
         then
           echo "success on compile $d.$m"
           ./test >> ../../../cpp-report.txt 2>&1
           if [ $? -eq 0 ]
           then
             echo $d.${tc%.*} successful
             echo $d.${tc%.*} successful >> ../../../cpp-report.txt
           else
             echo ${tc%.*} failed
             echo $d.${tc%.*} failed >> ../../../cpp-report.txt
           fi
         else
           echo "failure on compile $d.$m"
           echo "failure on compile $d.$m" >> ../../../cpp-report.txt
         fi
      cd ..
    done
  cd ..
done

echo "-------------"                >> ../cpp-report.txt
echo "FINALLY DONE"                 >> ../cpp-report.txt
echo "-------------"                >> ../cpp-report.txt

ls -l ../cpp-report.txt
