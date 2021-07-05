module velka.compiler {
	requires transitive velka.util;
	requires transitive velka.types;
	requires transitive velka.core;
	requires transitive velka.parser;
	requires transitive velka.clojure;
	exports velka.compiler;
}
