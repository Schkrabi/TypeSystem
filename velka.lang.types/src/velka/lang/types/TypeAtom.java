package velka.lang.types;

import java.util.Set;
import java.util.TreeSet;
import java.util.function.Function;

import velka.lang.util.AppendableException;

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
	public Type apply(Substitution s) {
		return this;
	}

	/**
	 * Creates clojure name of the type constructor
	 * 
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

	@Override
	public Substitution unifyTypeWith(Type other) throws AppendableException {
		if (other instanceof TypeVariable || other instanceof RepresentationOr) {
			return other.unifyTypeWith(this);
		}
		if (other instanceof TypeAtom) {
			if (TypeAtom.isSameBasicType(this, (TypeAtom) other)) {
				return Substitution.EMPTY;
			}
		}
		throw new TypesDoesNotUnifyException(this, other);
	}

	@Override
	public Substitution unifyRepresentationWith(Type other) throws AppendableException {
		if (other instanceof TypeVariable || other instanceof RepresentationOr) {
			return other.unifyRepresentationWith(this);
		}
		if (other instanceof TypeAtom) {
			if (this.equals(other)) {
				return Substitution.EMPTY;
			}
		}
		throw new TypesDoesNotUnifyException(this, other);
	}

	@Override
	public String clojureTypeRepresentation() {
		return "(new velka.lang.types.TypeAtom " + this.name.toClojureRepresentation() + " "
				+ this.representation.toClojureRepresentation() + ")";
	}

	@Override
	public Type uniteRepresentationsWith(Type other) throws AppendableException {
		if (other instanceof RepresentationOr || other instanceof TypeVariable) {
			return other.uniteRepresentationsWith(this);
		}
		if (!(other instanceof TypeAtom) || !(TypeAtom.isSameBasicType(this, (TypeAtom) other))) {
			throw new AppendableException("Cannot unite types " + this + " " + other);
		}
		return new TypeAtom(this.name, TypeRepresentation.WILDCARD);
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

	/**
	 * Type of List
	 */
	public static final TypeAtom TypeList = new TypeAtom(TypeName.LIST, TypeRepresentation.WILDCARD);
	public static final TypeAtom TypeListNative = new TypeAtom(TypeName.LIST, TypeRepresentation.NATIVE);

	@Override
	public Type map(Function<Type, Type> fun) throws AppendableException {
		return fun.apply(this);
	}
}
