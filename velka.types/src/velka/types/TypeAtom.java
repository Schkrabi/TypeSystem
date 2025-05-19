package velka.types;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.BiFunction;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import velka.util.ClojureHelper;
import velka.util.NameGenerator;
import velka.util.annotations.LangbaseType;
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
	
	public static final String conversionCostCljMetaName = ":conversion-cost";
	

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
		if(this == o) return true;
		if (o instanceof TypeAtom) {
			return this.name.equals(((TypeAtom) o).name) && this.representation.equals(((TypeAtom) o).representation);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return 31 * this.name.name.hashCode() + this.representation.name.hashCode();
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
	@LangbaseType
	public static final TypeAtom TypeBool = new TypeAtom(TypeName.BOOL, TypeRepresentation.WILDCARD) {
		@Override
		public String clojureTypeRepresentation() {
			return "velka.types.TypeAtom/TypeBool";
		}
	};
	@LangbaseType
	public static final TypeAtom TypeBoolNative = new TypeAtom(TypeName.BOOL, TypeRepresentation.NATIVE) {
		@Override
		public String clojureTypeRepresentation() {
			return "velka.types.TypeAtom/TypeBoolNative";
		}
	};

	/**
	 * Type of Integer
	 */
	@LangbaseType
	public static final TypeAtom TypeInt = new TypeAtom(TypeName.INT, TypeRepresentation.WILDCARD){
		@Override
		public String clojureTypeRepresentation() {
			return "velka.types.TypeAtom/TypeInt";
		}
	};
	@LangbaseType
	public static final TypeAtom TypeIntNative = new TypeAtom(TypeName.INT, TypeRepresentation.NATIVE) {
		@Override
		public String clojureTypeRepresentation() {
			return "velka.types.TypeAtom/TypeIntNative";
		}
	};
	@LangbaseType
	public static final TypeAtom TypeIntString = new TypeAtom(TypeName.INT, TypeRepresentation.STRING) {
		@Override
		public String clojureTypeRepresentation() {
			return "velka.types.TypeAtom/TypeIntString";
		}
	};
	@LangbaseType
	public static final TypeAtom TypeIntRoman = new TypeAtom(TypeName.INT, TypeRepresentation.ROMAN) {
		@Override
		public String clojureTypeRepresentation() {
			return "velka.types.TypeAtom/TypeIntRoman";
		}
	};

	/**
	 * Type of String
	 */
	@LangbaseType
	public static final TypeAtom TypeString = new TypeAtom(TypeName.STRING, TypeRepresentation.WILDCARD) {
		@Override
		public String clojureTypeRepresentation() {
			return "velka.types.TypeAtom/TypeString";
		}
	};
	@LangbaseType
	public static final TypeAtom TypeStringNative = new TypeAtom(TypeName.STRING, TypeRepresentation.NATIVE) {
		@Override
		public String clojureTypeRepresentation() {
			return "velka.types.TypeAtom/TypeStringNative";
		}
	};

	/**
	 * Type of Double
	 */
	@LangbaseType
	public static final TypeAtom TypeDouble = new TypeAtom(TypeName.DOUBLE, TypeRepresentation.WILDCARD) {
		@Override
		public String clojureTypeRepresentation() {
			return "velka.types.TypeAtom/TypeDouble";
		}
	};
	@LangbaseType
	public static final TypeAtom TypeDoubleNative = new TypeAtom(TypeName.DOUBLE, TypeRepresentation.NATIVE) {
		@Override
		public String clojureTypeRepresentation() {
			return "velka.types.TypeAtom/TypeDoubleNative";
		}
	};

	/**
	 * Type of List
	 */
	@LangbaseType
	public static final TypeAtom TypeList = new TypeAtom(TypeName.LIST, TypeRepresentation.WILDCARD) {
		@Override
		public String clojureTypeRepresentation() {
			return "velka.types.TypeAtom/TypeList";
		}
	};
	@LangbaseType
	public static final TypeAtom TypeListNative = new TypeAtom(TypeName.LIST, TypeRepresentation.NATIVE) {
		@Override
		public String clojureTypeRepresentation() {
			return "velka.types.TypeAtom/TypeListNative";
		}
	};
	
	/**
	 * Type of Set
	 */
	@LangbaseType
	public static final TypeAtom TypeSet = new TypeAtom(TypeName.SET, TypeRepresentation.WILDCARD) {
		@Override
		public String clojureTypeRepresentation() {
			return "velka.types.TypeAtom/TypeSet";
		}
	};
	
	@LangbaseType
	public static final TypeAtom TypeMap = new TypeAtom(new TypeName("Map"), TypeRepresentation.WILDCARD) {
		@Override
		public String clojureTypeRepresentation() {
			return "velka.types.TypeAtom/TypeMap";
		}
	};
	
	@LangbaseType
	public static final TypeAtom TypeMapTree = new TypeAtom(new TypeName("Map"), new TypeRepresentation("Tree")) {
		@Override
		public String clojureTypeRepresentation() {
			return "velka.types.TypeAtom/TypeMapTree";
		}
	};
	
	@LangbaseType
	public static final TypeAtom TypeScannerNative = new TypeAtom(new TypeName("Scanner"), TypeRepresentation.NATIVE)  {
		@Override
		public String clojureTypeRepresentation() {
			return "velka.types.TypeAtom/TypeScannerNative";
		}
	};
	/**
	 * Type for list iterator
	 */
	@LangbaseType
	public final static TypeAtom TypeListIterator = new TypeAtom(new TypeName("ListIterator"), TypeRepresentation.NATIVE) {
		@Override
		public String clojureTypeRepresentation() {
			return "velka.types.TypeAtom/TypeListIterator";
		}
	};
	/**
	 * Type of java linked list in velka
	 */
	@LangbaseType
	public final static TypeAtom TypeListJavaLinked = new TypeAtom(TypeName.LIST, new TypeRepresentation("JavaLinked")) {
		@Override
		public String clojureTypeRepresentation() {
			return "velka.types.TypeAtom/TypeListJavaLinked";
		}
	};

	/**
	 * Type of Set:BitSet in Velka
	 */
	@LangbaseType
	public static final TypeAtom TypeSetBitSet = new TypeAtom(TypeName.SET, new TypeRepresentation("BitSet")) {
		@Override
		public String clojureTypeRepresentation() {
			return "velka.types.TypeAtom/TypeSetBitSet";
		}
	};
	
	/** Type of Set:Tree */
	@LangbaseType
	public static final TypeAtom TypeSetTree = new TypeAtom(TypeName.SET, new TypeRepresentation("Tree")) {
		@Override
		public String clojureTypeRepresentation() {
			return "velka.types.TypeAtom/TypeSetTree";
		}
	};
	
	/** Type of Set:Hash */
	@LangbaseType
	public static final TypeAtom TypeSetHash = new TypeAtom(TypeName.SET, new TypeRepresentation("Hash")) {
		@Override
		public String clojureTypeRepresentation() {
			return "velka.types.TypeAtom/TypeSetHash";
		}
	};
	
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
	
	private static Class<?> listItrCl = (new java.util.LinkedList<Object>()).listIterator().getClass();
	private static Class<?> immListCl = List.of().getClass();
	private static Class<?> immSetCl = Set.of().getClass();
	private static Class<?> emptyVavRstreamCl = io.vavr.collection.Stream.empty().getClass();
	private static Class<?> headVavrStreamCl = io.vavr.collection.Stream.empty().prepend(null).getClass();
	
	public static Type javaClassToType(Class<?> clazz) {
		if(Object.class == clazz) return new TypeVariable(NameGenerator.next());
		else if(byte.class == clazz) return TypeAtom.TypeIntNative;
		else if(short.class == clazz) return TypeAtom.TypeIntNative;
		else if(int.class == clazz) return TypeAtom.TypeIntNative;
		else if(long.class == clazz) return TypeAtom.TypeIntNative;
		else if(float.class == clazz) return TypeAtom.TypeDoubleNative;
		else if(double.class == clazz) return TypeAtom.TypeDoubleNative;
		else if(boolean.class == clazz) return TypeAtom.TypeBoolNative;
		else if(void.class == clazz) return TypeTuple.EMPTY_TUPLE;
		else if(java.lang.Byte.class == clazz) return TypeAtom.TypeIntNative;
		else if(java.lang.Short.class == clazz) return TypeAtom.TypeIntNative;
		else if(java.lang.Integer.class == clazz) return TypeAtom.TypeIntNative;
		else if(java.lang.Long.class == clazz) return TypeAtom.TypeIntNative;
		else if(java.lang.Float.class == clazz) return TypeAtom.TypeDoubleNative;
		else if(java.lang.Double.class == clazz) return TypeAtom.TypeDoubleNative;
		else if(java.lang.String.class == clazz) return TypeAtom.TypeStringNative;
		else if(java.lang.Boolean.class == clazz) return TypeAtom.TypeBoolNative;
		else if(io.vavr.collection.Stream.class == clazz) return TypeListNative;
		else if(emptyVavRstreamCl == clazz) return TypeListNative;
		else if(headVavrStreamCl == clazz) return TypeListNative;
		//TODO remove or replace
		else if(java.util.Collection.class == clazz) return TypeListNative;
		else if(java.util.LinkedList.class == clazz) return TypeListJavaLinked;
		else if(java.util.List.class == clazz) return TypeListNative;
		else if(java.util.ArrayList.class == clazz) return TypeListNative;
		//TODO remove or replace END
		else if(java.util.ListIterator.class == clazz) return TypeListIterator;
		else if(java.util.TreeMap.class == clazz) return TypeMapTree;
		else if(java.util.BitSet.class == clazz) return TypeSetBitSet;
		else if(java.util.Scanner.class == clazz) return TypeScannerNative;
		else if(java.util.TreeSet.class == clazz) return TypeSetTree;
		else if(java.util.HashSet.class == clazz) return TypeSetHash;
		else if(listItrCl == clazz) return TypeListIterator;
		else if(immListCl == clazz) return TypeListNative;
		else if(immSetCl == clazz) return TypeSetTree;
		return null;
	}

	@Override
	public boolean isRepUncertain() {
		return this.representation.equals(TypeRepresentation.WILDCARD);
	}
}
