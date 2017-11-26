package types;

import java.util.Set;
import java.util.TreeSet;

import expression.IntBinary;
import expression.IntRoman;
import expression.IntString;
import expression.LitBoolean;
import expression.LitDouble;
import expression.LitString;
import expression.Literal;

/**
 * Class for concrete types and complete types representations
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public abstract class TypeConcrete extends Type {
	/**
	 * Name of the type
	 */
	public final String name;
	/**
	 * Representation of the type (if applicable)
	 */
	public final String representation;
	/**
	 * Associated literal class of the representation/complete type
	 */
	public final Class<? extends Literal> implementation;
	
	protected final Map<String, >

	public TypeConcrete(String name, Class<? extends Literal> implementation) {
		this.name = name;
		this.representation = "";
		this.implementation = implementation;
	}

	public TypeConcrete(String name, Class<? extends Literal> implementation, String representation) {
		this.name = name;
		this.representation = representation;
		this.implementation = implementation;
	}

	@Override
	public String toString() {
		if (this.representation == "") {
			return this.name;
		} else {
			return this.name + ":" + this.representation;
		}
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof TypeConcrete)) {
			return false;
		}
		TypeConcrete other = (TypeConcrete) o;
		return this.name.equals(other.name) && this.representation.equals(other.representation);
	}

	/**
	 * Returns true if the other object is the same basic type (ommiting
	 * representations), otherwise returns false
	 * 
	 * @param other
	 *            Type
	 * @return true or false
	 */
	public boolean isSameBasicType(TypeConcrete other) {
		return this.name.equals(other.name);
	}

	/**
	 * Type of Bool
	 */
	public static final TypeConcrete TypeBool = new TypeConcrete("Bool", LitBoolean.class);
	/**
	 * Type of Int represented by Binary integer
	 */
	public static final TypeConcrete TypeIntBinary = new TypeConcrete("Int", IntBinary.class, "Binary");
	/**
	 * Type of Int represented by decimal string
	 */
	public static final TypeConcrete TypeIntString = new TypeConcrete("Int", IntString.class, "String");
	/**
	 * Type of Int reprented by Roman number string
	 */
	public static final TypeConcrete TypeIntRoman = new TypeConcrete("Int", IntRoman.class, "Roman");
	/**
	 * Type of String
	 */
	public static final TypeConcrete TypeString = new TypeConcrete("String", LitString.class);
	/**
	 * Type of Double
	 */
	public static final TypeConcrete TypeDouble = new TypeConcrete("Double", LitDouble.class);

	@Override
	public Set<TypeVariable> getUnconstrainedVariables() {
		Set<TypeVariable> s = new TreeSet<TypeVariable>();
		return s;
	}

	@Override
	public int compareTo(Type o) {
		if (!(o instanceof TypeConcrete)) {
			return super.compareTo(o);
		}
		TypeConcrete other = (TypeConcrete) o;
		int cmp = this.name.compareTo(other.name);
		if (cmp != 0) {
			return cmp;
		}

		return this.representation.compareTo(other.representation);
	}
	
	public abstract Literal instantiateLiteral(Object value) throws Exception;
}
