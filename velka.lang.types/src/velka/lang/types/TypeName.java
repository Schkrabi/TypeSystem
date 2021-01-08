package velka.lang.types;

/**
 * . This class holds names for type atoms
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class TypeName implements Comparable<TypeName> {
	/**
	 * Identifier of the type name
	 */
	public final String name;

	public TypeName(String name) {
		this.name = name;
	}

	@Override
	public String toString() {
		return this.name;
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof TypeName) {
			return this.name.equals(((TypeName) other).name);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.name.hashCode();
	}

	@Override
	public int compareTo(TypeName arg0) {
		return this.name.compareTo(arg0.name);
	}
	
	/**
	 * Creates code to create instance of this type in clojure
	 * @return string with code
	 */
	public String toClojureRepresentation() {
		return "(new velka.lang.types.TypeName \"" + this.name + "\")";
	}

	public static final TypeName INT = new TypeName("Int");
	public static final TypeName STRING = new TypeName("String");
	public static final TypeName BOOL = new TypeName("Bool");
	public static final TypeName DOUBLE = new TypeName("Double");
	public static final TypeName LIST = new TypeName("List");
	public static final TypeName TYPE = new TypeName("Type");
}
