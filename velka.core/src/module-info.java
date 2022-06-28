module velka.core {
	requires transitive velka.util;
	requires transitive velka.types;
	requires java.logging;
	exports velka.core.abstraction;
	exports velka.core.application;
	exports velka.core.expression;
	exports velka.core.exceptions;
	exports velka.core.interpretation;
	exports velka.core.langbase;
	exports velka.core.literal;
}