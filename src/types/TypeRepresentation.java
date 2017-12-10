package types;

import expression.Expression;
import expression.IntRoman;
import expression.IntString;
import expression.Literal;

public abstract class TypeRepresentation extends TypeConcrete {
	/**
	 * Base Type of this representation
	 */
	public final TypeConcrete baseType;

	public TypeRepresentation(String name, Class<? extends Literal> implementation, TypeConcrete baseType) {
		super(name, implementation);
		this.baseType = baseType;
	}

	@Override
	public String toString() {
		return this.baseType.name + ":" + this.name;
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof TypeRepresentation)) {
			return false;
		}
		TypeRepresentation other = (TypeRepresentation)o;
		return this.baseType.equals(other) && this.name == other.name;
	}

	@Override
	public int compareTo(Type o) {
		if (!(o instanceof TypeRepresentation)) {
			return super.compareTo(o);
		}
		TypeRepresentation other = (TypeRepresentation)o;
		int cmp = this.baseType.compareTo(other.baseType);
		if(cmp != 0) {
			return cmp;
		}
		return this.name.compareTo(other.name);
	}

	@Override
	public boolean isSameBasicType(Type o) {
		if (o instanceof TypeRepresentation) {
			TypeRepresentation other = (TypeRepresentation) o;
			return this.baseType.equals(other.baseType);
		}
		if (o instanceof TypeConcrete) {
			TypeConcrete other = (TypeConcrete) o;
			return this.baseType.equals(other);
		}
		return false;
	}
	
	@Override
	public Expression convertTo(Expression expr, Type toType)
			throws Exception {
		return this.baseType.convertTo(expr, toType);
	}

	/**
	 * String representation of int
	 */
	public static final TypeRepresentation TypeIntString = new TypeRepresentation("String", IntString.class,
			TypeConcrete.TypeInt) {
		@Override
		public Literal instantiateLiteral(Object value) throws Exception {
			if (!(value instanceof String)) {
				this.throwInitializationError(TypeConcrete.TypeInt.getClass(), value);
			}
			String s = (String) value;
			return new IntString(s);
		}
	};

	/**
	 * Roman representation of int
	 */
	public static final TypeRepresentation TypeIntRoman = new TypeRepresentation("Roman", IntRoman.class,
			TypeConcrete.TypeInt) {
		@Override
		public Literal instantiateLiteral(Object value) throws Exception {
			if (!(value instanceof String)) {
				this.throwInitializationError(TypeConcrete.TypeInt.getClass(), value);
			}
			String s = (String) value;
			return new IntRoman(s);
		}
	};
}
