module velka.compiler {
	requires transitive velka.util;
	requires transitive velka.types;
	requires transitive velka.core;
	requires transitive velka.parser;
	requires transitive velka.clojure;
	requires java.logging;
 	requires static org.junit.jupiter.api;
	exports velka.compiler;
}
