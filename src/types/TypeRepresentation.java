package types;

public class TypeRepresentation extends TypeConcrete {
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
		TypeRepresentation other = (TypeRepresentation) o;
		return this.baseType.equals(other.baseType) && this.name == other.name;
	}

	@Override
	public int compareTo(Type o) {
		if (!(o instanceof TypeRepresentation)) {
			return super.compareTo(o);
		}
		TypeRepresentation other = (TypeRepresentation) o;
		int cmp = this.baseType.compareTo(other.baseType);
		if (cmp != 0) {
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
	public TypeConcrete baseType(){
		return this.baseType;
	}

	/**
	 * String representation of int
	 */
	public static final TypeRepresentation TypeIntString = new TypeRepresentation("String", TypeConcrete.TypeInt);

	/**
	 * Roman representation of int
	 */
	public static final TypeRepresentation TypeIntRoman = new TypeRepresentation("Roman", TypeConcrete.TypeInt);
}
