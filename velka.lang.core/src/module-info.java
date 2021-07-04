module velka.lang.core {
	requires transitive velka.lang.util;
	requires transitive velka.lang.types;
	requires java.logging;
	exports velka.lang.abstraction;
	exports velka.lang.application;
	exports velka.lang.conversions;
	exports velka.lang.expression;
	exports velka.lang.coreExceptions;
	exports velka.lang.interpretation;
	exports velka.lang.langbase;
	exports velka.lang.literal;
}