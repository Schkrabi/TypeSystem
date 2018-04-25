package types;

import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import expression.Application;
import expression.TypeConstructionLambda;
import interpretation.Environment;
import expression.Expression;
import expression.Tuple;
import util.AppendableException;

/**
 * Class for concrete types
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class TypeConcrete extends Type {
	/**
	 * Name of the type
	 */
	public final String name;

	/**
	 * Map for type converting
	 */
	private Map<TypeConcrete, TypeConstructionLambda> conversionTable = new TreeMap<TypeConcrete, TypeConstructionLambda>();

	public TypeConcrete(String name) {
		this.name = name;
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

	public void addConversion(TypeConcrete toType, TypeConstructionLambda conversionConstructor)
			throws AppendableException {
		if (this.conversionTable.containsKey(toType)) {
			throw new AppendableException(
					"Conversion of " + this.getClass().getName() + " to " + toType.name + " already exists.");
		}
		
		if(this.baseType() != toType.baseType()){
			throw new AppendableException("Can only create conversions between the same base types!");
		}
		
		this.conversionTable.put(toType, conversionConstructor);
	}

	private Expression instantiateConversionToType(TypeConcrete type, Expression arg) throws Exception {
		TypeConstructionLambda constructor = this.conversionTable.get(type);
		
		if (constructor == null) {
			throw new Exception(
					"No conversion from " + this + " to type " + type + " exists");
		}

		return new Application(constructor, new Tuple(new Expression[]{arg}));
	}

	@Override
	public Expression convertTo(Expression expr, Type toType) throws Exception {
		if(toType instanceof TypeVariable){
			return expr;
		}		
		if (!(toType instanceof TypeConcrete)) {
			this.throwConversionError(expr, toType);
		}
		if (expr.getType() != this) {
			throw new Exception("Invalid converison of " + expr.getType() + " carried out by " + this);
		}

		TypeConcrete t = (TypeConcrete) toType;

		if (toType == this) {
			return expr;
		}

		Expression e = this.instantiateConversionToType(t, expr);
		e.infer(new Environment());
		return e;
	}

	@Override
	public Expression convertToDefaultRepresentation(Expression expr) throws Exception {
		if (expr.getType() != this) {
			throw new Exception("Invalid converison of " + expr.getType() + " carried out by " + this);
		}
		return expr;
	}
	
	/**
	 * Get the base type of this type or type representation
	 * @return
	 */
	public TypeConcrete baseType(){
		return this;
	}

	/**
	 * Type of Bool
	 */
	public static final TypeConcrete TypeBool = new TypeConcrete("Bool");
	/**
	 * Type of Integer
	 */
	public static final TypeConcrete TypeInt = new TypeConcrete("Int");

	/**
	 * Type of String
	 */
	public static final TypeConcrete TypeString = new TypeConcrete("String");
	/**
	 * Type of Double
	 */
	public static final TypeConcrete TypeDouble = new TypeConcrete("Double");
}
