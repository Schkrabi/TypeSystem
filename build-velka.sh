#! /bin/bash

rm ./lib/velka.lang.util.jar
rm ./lib/velka.lang.types.jar
rm ./lib/velka.lang.compiler.jar

echo Building velka.lang.util
cd ./velka.lang.util
ant
cd ..
cp ./velka.lang.util/jar/velka.lang.util.jar ./lib/velka.lang.util.jar

echo Building velka.lang.types
cd ./velka.lang.types
ant
cd ..
cp ./velka.lang.types/jar/velka.lang.types.jar ./lib/velka.lang.types.jar

echo Building velka.lang.compiler
cd ./velka.lang.compiler
ant
cd ..
cp ./velka.lang.compiler/jar/velka.lang.compiler.jar ./lib/velka.lang.compiler.jar

echo velka.lang build finished
