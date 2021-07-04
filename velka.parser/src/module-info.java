module velka.parser {
	requires transitive velka.util;
	requires transitive velka.types;
	requires transitive velka.core;
	requires antlr;
	exports velka.parser;
	exports velka.parser.exceptions;
}