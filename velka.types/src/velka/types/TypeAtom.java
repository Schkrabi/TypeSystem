package velka.types;

import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.BiFunction;

import velka.util.ClojureCoreSymbols;
import velka.util.ClojureHelper;
import velka.types.TypeAtom;
import velka.util.AppendableException;

/**
 * Class for type atoms
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class TypeAtom extends TerminalType {

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
	public Set<TypeVariable> getVariables() {
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
	public Optional<Substitution> unifyTypeWith(Type other) {
		if (other instanceof TypeVariable || other instanceof RepresentationOr) {
			return other.unifyTypeWith(this);
		}
		if (other instanceof TypeAtom) {
			if (TypeAtom.isSameBasicType(this, (TypeAtom) other)) {
				return Optional.of(Substitution.EMPTY);
			}
		}
		return Optional.empty();
	}

	@Override
	public Optional<Substitution> unifyRepresentationWith(Type other) {
		if (other instanceof TypeVariable || other instanceof RepresentationOr) {
			return other.unifyRepresentationWith(this);
		}
		if (other instanceof TypeAtom) {
			if (this.equals(other)) {
				return Optional.of(Substitution.EMPTY);
			}
			TypeAtom o = (TypeAtom)other;
			if(this.name.equals(o.name)
					&& (this.representation.equals(TypeRepresentation.WILDCARD)
							|| o.representation.equals(TypeRepresentation.WILDCARD))) {
				return Optional.of(Substitution.EMPTY);
			}
		}
		return Optional.empty();
	}

	@Override
	public String clojureTypeRepresentation() {
		String code = ClojureHelper.instantiateJavaClass(
				this.getClass(),
				this.name.toClojureRepresentation(),
				this.representation.toClojureRepresentation());
		return code;
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
	public static final TypeAtom TypeBool = new TypeAtom(TypeName.BOOL, TypeRepresentation.WILDCARD) ;
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
	
	/**
	 * Type of Set
	 */
	public static final TypeAtom TypeSet = new TypeAtom(TypeName.SET, TypeRepresentation.WILDCARD);
	
	/**
	 * Type of TypeSymbol
	 */
	public static final TypeAtom TypeType = new TypeAtom(TypeName.TYPE, TypeRepresentation.WILDCARD);
	public static final TypeAtom TypeTypeNative = new TypeAtom(TypeName.TYPE, TypeRepresentation.NATIVE);

	@Override
	public Type replaceVariable(TypeVariable replaced, TypeVariable replacee) throws AppendableException {
		return this;
	}

	/**
	 * Creates code to add conversion to global variable
	 * 
	 * @param fromType from type of conversion
	 * @param toType to type of conversion
	 * @param conversionCode code for conversion
	 * @return String with code
	 */
	public static String addConversionToGlobalTable(TypeAtom fromType, TypeAtom toType, String conversionCode) {
		String code = ClojureHelper.rebindGlobalVariable(ClojureCoreSymbols.atomicConversionMapClojureSymbol_full,
				ClojureHelper.applyClojureFunction("assoc",
						ClojureCoreSymbols.atomicConversionMapClojureSymbol_full,
						makeAtomicConversionRecord(fromType, toType, conversionCode)));
		
		return code;
	}

	/**
	 * Creates record for atomic conversion map
	 * 
	 * @param fromType from type of conversion
	 * @param toType to type of conversion
	 * @param conversionCode code for conversion
	 * @return String with code
	 */
	public static String makeAtomicConversionRecord(TypeAtom fromType, TypeAtom toType, String conversionCode) {
		StringBuilder sb = new StringBuilder();
		sb.append(ClojureHelper.clojureVectorHelper(fromType.clojureTypeRepresentation(), toType.clojureTypeRepresentation()));
		sb.append(" ");
		sb.append(conversionCode);
		return sb.toString();
	}

	@Override
	public boolean doCanConvertTo(Type other, BiFunction<TypeAtom, TypeAtom, Boolean> atomCheck) {
		if(!(other instanceof TypeAtom)) {
			return false;
		}
		if(this.equals(other)) {
			return true;
		}
		return atomCheck.apply(this, (TypeAtom) other);
	}
}
