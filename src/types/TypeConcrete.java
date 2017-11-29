package types;

import java.util.Set;
import java.util.TreeSet;

import expression.IntBinary;
import expression.LitBoolean;
import expression.LitDouble;
import expression.LitString;
import expression.Literal;

/**
 * Class for concrete types
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
	 * Associated literal class of the representation/complete type
	 */
	public final Class<? extends Literal> implementation;

	public TypeConcrete(String name, Class<? extends Literal> implementation) {
		this.name = name;
		this.implementation = implementation;
	}

	/**
	 * Returns true if this and other are TypeConcrete or TypeRepresentation and
	 * represents the same basic type
	 * 
	 * @param other
	 *            the other type
	 * @return true or false
	 */
	protected void throwInitializationError(Class<? extends TypeConcrete> type, Object value) throws Exception {
		throw new Exception("Instantiating " + type.getName() + ":" + this.name + "with value of type "
				+ value.getClass().getName() + " of value " + value.toString());
	}

	/**
	 * Instantiates the literal of given type. Must return literal of implementation
	 * class.
	 * 
	 * @param value
	 *            Initialization value
	 * @return Literal object
	 * @throws Exception
	 *             thrown if instantiated with incorrect value
	 * @see TypeConcrete.implementation
	 */
	public abstract Literal instantiateLiteral(Object value) throws Exception;

	@Override
	public String toString() {
		return this.name;
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof TypeConcrete)) {
			return false;
		}
		TypeConcrete other = (TypeConcrete) o;
		return this.name.equals(other.name);
	}

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
		return this.name.compareTo(other.name);
	}

	public boolean isSameBasicType(Type o) {
		if (o instanceof TypeRepresentation) {
			TypeRepresentation other = (TypeRepresentation) o;
			return this.equals(other.baseType);
		}
		if (o instanceof TypeConcrete) {
			TypeConcrete other = (TypeConcrete) o;
			return this.equals(other);
		}
		return false;
	}

	/**
	 * Type of Bool
	 */
	public static final TypeConcrete TypeBool = new TypeConcrete("Bool", LitBoolean.class) {
		@Override
		public Literal instantiateLiteral(Object value) throws Exception {
			if (!(value instanceof Boolean)) {
				this.throwInitializationError(TypeConcrete.TypeBool.getClass(), value);
			}
			Boolean b = (Boolean) value;
			return b ? LitBoolean.TRUE : LitBoolean.FALSE;
		}
	};
	/**
	 * Type of Integer
	 */
	public static final TypeConcrete TypeInt = new TypeConcrete("Int", IntBinary.class) {

		@Override
		public Literal instantiateLiteral(Object value) throws Exception {
			if (!(value instanceof Integer)) {
				this.throwInitializationError(TypeConcrete.TypeInt.getClass(), value);
			}
			Integer i = (Integer) value;
			return new IntBinary(i.intValue());
		}
	};

	/**
	 * Type of String
	 */
	public static final TypeConcrete TypeString = new TypeConcrete("String", LitString.class) {

		@Override
		public Literal instantiateLiteral(Object value) throws Exception {
			if (!(value instanceof String)) {
				this.throwInitializationError(TypeConcrete.TypeString.getClass(), value);
			}
			String s = (String) value;
			return new LitString(s);
		}

	};
	/**
	 * Type of Double
	 */
	public static final TypeConcrete TypeDouble = new TypeConcrete("Double", LitDouble.class) {

		@Override
		public Literal instantiateLiteral(Object value) throws Exception {
			if (!(value instanceof Double)) {
				this.throwInitializationError(TypeConcrete.TypeDouble.getClass(), value);
			}
			Double d = (Double) value;
			return new LitDouble(d);
		}
	};
}
