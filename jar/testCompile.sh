#!/bin/bash

COMPILER=./Compiler.jar
TESTDIR=../Tests/Code/
CLJDIR=../Tests/CLJ/
INTPDIR=../Tests/INTP/
CMPLDIR=../Tests/CMPL/
DIFFDIR=../Tests/DIFF/

rm ${CLJDIR}*
rm ${INPTDIR}*
rm ${CMPLDIR}*
rm ${DIFFDIR}*

for TESTFILE in ${TESTDIR}*
do
    echo "Processing $TESTFILE file"
    TESTNAME=$(basename $TESTFILE)
    
    #Save result of code interpretation
    INTPFILE=${INTPDIR}${TESTNAME}.rsl
    java -jar $COMPILER $TESTFILE > $INTPFILE
    
    #Save result of code compilation and clojure interpretation
    CLJFILE=${CLJDIR}${TESTNAME}.clj
    CMPLFILE=${CMPLDIR}${TESTNAME}.rsl
    java -jar $COMPILER $TESTFILE $CLJFILE
    clojure $CLJFILE > $CMPLFILE
    
    #Check diff and save it if there is a difference
    DIFF=$(diff -q $INTPFILE $CMPLFILE)
    if [ "$DIFF" ]
    then
        DIFFFILE=${DIFFDIR}${TESTNAME}.diff
        echo "Issue on file $TESTFILE, see $DIFFFILE"
        diff -y $INTPFILE $CMPLFILE > $DIFFFILE
    fi
done
