package types;

import util.RomanNumbers;
import expression.Expression;
import expression.LitString;
import expression.Literal;

public abstract class TypeRepresentation extends TypeConcrete {
	/**
	 * Base Type of this representation
	 */
	public final TypeConcrete baseType;

	public TypeRepresentation(String name, TypeConcrete baseType) {
		super(name);
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
	public Expression convertToDefaultRepresentation(Expression expr) throws Exception{
		if(expr.getType() != this) {
			throw new Exception("Invalid converison of " + expr.getType() + " carried out by " + this);
		}
		return this.convertTo(expr, this.baseType);
	}

	/**
	 * String representation of int
	 */
	public static final TypeRepresentation TypeIntString = new TypeRepresentation("String",
			TypeConcrete.TypeInt) {
		
		@Override
		public Literal instantiateLiteral(Object value) throws Exception {
			if (!(value instanceof String)) {
				this.throwInitializationError(TypeConcrete.TypeInt.getClass(), value);
			}
			String s = (String) value;
			Literal l = new LitString(s);
			l.setLiteralType(this);
			return l;
		}
	};

	/**
	 * Roman representation of int
	 */
	public static final TypeRepresentation TypeIntRoman = new TypeRepresentation("Roman",
			TypeConcrete.TypeInt) {
		
		@Override
		public Literal instantiateLiteral(Object value) throws Exception {
			if (!(value instanceof String)) {
				this.throwInitializationError(TypeConcrete.TypeInt.getClass(), value);
			}
			String s = (String) value;
			if(!RomanNumbers.check(s)){
				throw new Exception("Invalid string " + s + " used for roman number.");
			}			
			Literal l = new LitString(s);
			l.setLiteralType(this);
			return l;
		}
	};
}
