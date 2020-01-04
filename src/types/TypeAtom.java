package types;

import java.util.Set;
import java.util.TreeSet;

import expression.Expression;
import semantic.TypeEnvironment;
import util.AppendableException;

/**
 * Class for type atoms
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class TypeAtom extends Type {

	public final TypeName name;
	public final TypeRepresentation representation;

	public TypeAtom(TypeName name, TypeRepresentation representation) {
		this.name = name;
		this.representation = representation;
	}

	@Override
	public String toString() {
		return this.name.toString() + ":" + this.representation.toString();
	}

	@Override
	public boolean equals(Object o) {
		if (o instanceof TypeAtom) {
			return this.name.equals(((TypeAtom) o).name) && this.representation.equals(((TypeAtom) o).representation);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.name.hashCode() * this.representation.hashCode();
	}

	@Override
	public int compareTo(Type o) {
		if (!(o instanceof TypeAtom)) {
			return super.compareTo(o);
		}
		TypeAtom other = (TypeAtom) o;
		int cmp = this.name.compareTo(other.name);
		if (cmp != 0)
			return cmp;
		return this.representation.compareTo(other.representation);
	}

	@Override
	public Set<TypeVariable> getUnconstrainedVariables() {
		return new TreeSet<TypeVariable>();
	}

	@Override
	public Expression convertTo(Expression expr, Type toType) throws AppendableException {
		if (toType instanceof TypeVariable || toType.equals(this)) {
			return expr;
		}
		if (!(toType instanceof TypeAtom) || !TypeAtom.isSameBasicType(this, (TypeAtom) toType)) {
			throw new ConversionException(this, toType, expr);
		}
		if(((TypeAtom)toType).representation.equals(TypeRepresentation.WILDCARD)) {
			return expr;
		}

		Expression e = TypeEnvironment.singleton.convertTo(expr, this, (TypeAtom)toType);

		return e;
	}
	
	@Override
	public String convertToClojure(String argument, Type toType) throws AppendableException {
		if (toType instanceof TypeVariable || toType.equals(this)) {
			return argument;
		}
		if (!(toType instanceof TypeAtom) || !TypeAtom.isSameBasicType(this, (TypeAtom) toType)) {
			throw new ClojureConversionException(this, toType, argument);
		}
		if(((TypeAtom)toType).representation.equals(TypeRepresentation.WILDCARD)) {
			return argument;
		}
		
		return "(" + TypeEnvironment.makeConversionName(this, (TypeAtom)toType) + " " + argument + ")";
	}

	@Override
	public Type removeRepresentationInfo() {
		return new TypeAtom(this.name, TypeRepresentation.WILDCARD);
	}
	
	@Override
	public Type apply(Substitution s) {
		return this;
	}
	
	/**
	 * Creates clojure name of the type constructor
	 * @return String
	 */
	public String clojureName() {
		return this.name.toString() + ":" + this.representation.toString();
	}

	/**
	 * Returns true if two type atoms are of the same type. Otherwise returns false.
	 * 
	 * @param t1 TypeAtom
	 * @param t2 TypeAtom
	 * @return true or false.
	 */
	public static boolean isSameBasicType(TypeAtom t1, TypeAtom t2) {
		return t1.name.equals(t2.name);
	}

	/**
	 * Type of Bool
	 */
	public static final TypeAtom TypeBool = new TypeAtom(TypeName.BOOL, TypeRepresentation.WILDCARD);
	public static final TypeAtom TypeBoolNative = new TypeAtom(TypeName.BOOL, TypeRepresentation.NATIVE);
	
	/**
	 * Type of Integer
	 */
	public static final TypeAtom TypeInt = new TypeAtom(TypeName.INT, TypeRepresentation.WILDCARD);
	public static final TypeAtom TypeIntNative = new TypeAtom(TypeName.INT, TypeRepresentation.NATIVE);
	public static final TypeAtom TypeIntString = new TypeAtom(TypeName.INT, TypeRepresentation.STRING);
	public static final TypeAtom TypeIntRoman = new TypeAtom(TypeName.INT, TypeRepresentation.ROMAN);

	/**
	 * Type of String
	 */
	public static final TypeAtom TypeString = new TypeAtom(TypeName.STRING, TypeRepresentation.WILDCARD);
	public static final TypeAtom TypeStringNative = new TypeAtom(TypeName.STRING, TypeRepresentation.NATIVE);
	
	/**
	 * Type of Double
	 */
	public static final TypeAtom TypeDouble = new TypeAtom(TypeName.DOUBLE, TypeRepresentation.WILDCARD);
	public static final TypeAtom TypeDoubleNative = new TypeAtom(TypeName.DOUBLE, TypeRepresentation.NATIVE);

	@Override
	public Substitution unifyWith(Type other) throws AppendableException {
		if(other instanceof TypeVariable
				|| other instanceof RepresentationOr) {
			return other.unifyWith(this);
		}
		if(other instanceof TypeAtom) {
			if(TypeAtom.isSameBasicType(this, (TypeAtom) other)) {
				return Substitution.EMPTY;
			}
		}
		throw new TypesDoesNotUnifyException(this, other);
	}
}
