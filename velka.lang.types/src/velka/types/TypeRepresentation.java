package velka.types;

import velka.types.TypeRepresentation;

/**
 * Class for names of type representations
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class TypeRepresentation implements Comparable<TypeRepresentation> {

	/**
	 * Name of the representation
	 */
	public final String name;

	public TypeRepresentation(String name) {
		this.name = name;
	}

	@Override
	public String toString() {
		return this.name;
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof TypeRepresentation) {
			return this.name.equals(((TypeRepresentation) other).name);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.name.hashCode();
	}

	@Override
	public int compareTo(TypeRepresentation arg0) {
		return this.name.compareTo(arg0.name);
	}
	
	/**
	 * Creates code to create instance of this type in clojure
	 * @return string with code
	 */
	public String toClojureRepresentation() {
		return "(new velka.lang.types.TypeRepresentation \"" + this.name + "\")";
	}

	/**
	 * Represents no concrete representation of type, for internal use only
	 */
	public static final TypeRepresentation WILDCARD = new TypeRepresentation("*");
	/**
	 * Represents type representations wrapped from Java
	 */
	public static final TypeRepresentation NATIVE = new TypeRepresentation("Native");
	/**
	 * Represents type representation in string
	 */
	public static final TypeRepresentation STRING = new TypeRepresentation("String");
	/**
	 * Represents type representations of integers as roman numbers
	 */
	public static final TypeRepresentation ROMAN = new TypeRepresentation("Roman");
}
