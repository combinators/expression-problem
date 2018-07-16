To retrieve stats on the individual Scala files,

DD/I    Domain Dependent/Independent
LD/I    Language Dependent/Independent
AD/I    Approach Dependent/Independent

Numbers as of 7-9-2018

DI:LI:AI   200
DI:LI:AD   0
DI:LD:AI   248
DI:LD:AD   1198
DD:LI:AI   185
DD:LI:AD   0
DD:LD:AI   876
DD:LD:AD   377


#!/bin/bash

echo -n "DI:LI:AI  "
wc -l `find . -name "*.scala" -exec grep "DI:LI:AI" {} \; -print | grep "/example/"`  | tail -1 | sed 's/total//'

## SKIP  since 0
##wc -l `find . -name "*.scala" -exec grep "DI:LI:AD" {} \; -print | grep "/example/"`
echo "DI:LI:AD   0"

echo -n "DI:LD:AI  "
wc -l `find . -name "*.scala" -exec grep "DI:LD:AI" {} \; -print | grep "/example/"`  | tail -1| sed 's/total//'

echo -n "DI:LD:AD  "
wc -l `find . -name "*.scala" -exec grep "DI:LD:AD" {} \; -print | grep "/example/"`  | tail -1| sed 's/total//'

echo -n "DD:LI:AI  "
wc -l `find . -name "*.scala" -exec grep "DD:LI:AI" {} \; -print | grep "/example/"`  | tail -1| sed 's/total//'

## SKIP since 0
##wc -l `find . -name "*.scala" -exec grep "DD:LI:AD" {} \; -print | grep "/example/"`
echo "DD:LI:AD   0"

echo -n "DD:LD:AI "
wc -l `find . -name "*.scala" -exec grep "DD:LD:AI" {} \; -print | grep "/example/"`  | tail -1| sed 's/total//'

echo -n "DD:LD:AD "
wc -l `find . -name "*.scala" -exec grep "DD:LD:AD" {} \; -print | grep "/example/"`  | tail -1| sed 's/total//'
