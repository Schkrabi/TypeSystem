module velka.core {
	requires transitive velka.util;
	requires transitive velka.types;
	requires velka.java;
	requires java.logging;
	requires java.base;
	requires com.sun.codemodel;
	
	exports velka.core.abstraction;
	exports velka.core.application;
	exports velka.core.expression;
	exports velka.core.exceptions;
	exports velka.core.interpretation;
	exports velka.core.interfaces;
	exports velka.core.langbase;
	exports velka.core.literal;
	exports velka.core.util;
}