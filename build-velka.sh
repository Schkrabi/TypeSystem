#! /bin/bash

rm ./lib/velka.util.jar
rm ./lib/velka.types.jar
rm ./lib/velka.core.jar
rm ./lib/velka.clojure.jar
rm ./lib/velka.parser.jar
rm ./lib/velka.compiler.jar

echo Building velka.util
cd ./velka.util
ant
cd ..
cp ./velka.util/jar/velka.util.jar ./lib/velka.util.jar

echo Building velka.types
cd ./velka.types
ant
cd ..
cp ./velka.types/jar/velka.types.jar ./lib/velka.types.jar

echo Building velka.core
cd ./velka.core
ant
cd ..
cp ./velka.core/jar/velka.core.jar ./lib/velka.core.jar

echo Building velka.lang.clojure
cd ./velka.clojure
ant
cd ..
cp ./velka.clojure/jar/velka.clojure.jar ./lib/velka.clojure.jar

echo Building velka.parser
cd ./velka.parser
ant
cd ..
cp ./velka.parser/jar/velka.parser.jar ./lib/velka.parser.jar

echo Building velka.compiler
cd ./velka.compiler
ant
cd ..
cp ./velka.compiler/jar/velka.compiler.jar ./lib/velka.compiler.jar

echo velka.lang build finished
