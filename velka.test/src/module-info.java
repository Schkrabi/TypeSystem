module velka.test {
	requires velka.util;
	requires velka.types;
	requires velka.core;
	requires velka.parser;
	requires velka.clojure;
	requires java.logging;
	requires velka.java;
	requires antlr;
 	requires static org.junit.jupiter.api;
	requires velka.compiler;
	requires com.palantir.javapoet;
	requires java.compiler;
	requires velka.java.generate;
	requires transitive com.sun.codemodel;
	requires io.vavr;
}