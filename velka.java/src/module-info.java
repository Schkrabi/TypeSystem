/**
 * 
 */
/**
 * 
 */
module velka.java {
	requires velka.util;
	requires transitive velka.types;
	requires transitive com.sun.codemodel;
	requires io.vavr;
	
	exports velka.java;
	exports velka.java.runtime;
}