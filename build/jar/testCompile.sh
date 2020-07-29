#!/bin/bash

COMPILER=./compiler.jar
TESTDIR=../../tests/Code/
CLJDIR=../../tests/CLJ/
INTPDIR=../../tests/INTP/
CMPLDIR=../../tests/CMPL/
DIFFDIR=../../tests/DIFF/

mkdir -p ${CLJDIR}
mkdir -p ${INTPDIR}
mkdir -p ${CMPLDIR}
mkdir -p ${DIFFDIR}

rm -f ${CLJDIR}*
rm -f ${INTPDIR}*
rm -f ${CMPLDIR}*
rm -f ${DIFFDIR}*

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