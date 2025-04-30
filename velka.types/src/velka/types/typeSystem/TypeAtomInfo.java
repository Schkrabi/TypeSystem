package velka.types.typeSystem;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import velka.types.Type;
import velka.types.TypeAtom;
import velka.types.TypeRepresentation;
import velka.types.TypeTuple;
import velka.util.IEvalueable;

/** Holds constructors and conversion for type atoms */
public class TypeAtomInfo {
	/** Type to which information is relatet */
	public final TypeAtom type;
	private final TypeSystem typeSystem;
	
	//record ConversionInfo(TypeAtom from, TypeAtom to, IEvalueable conversion, IEvalueable cost) {}
	
	private Map<TypeTuple, IEvalueable> constructors = new HashMap<TypeTuple, IEvalueable>();
	//private Map<TypeAtom, ConversionInfo> conversions = new HashMap<TypeAtom, ConversionInfo>();
	
	public TypeAtomInfo(TypeAtom type, TypeSystem typeSystem) {
		this.type = type;
		this.typeSystem = typeSystem;
	}
	
	/** Tries to find constructor with specific argument types */
	private IEvalueable findConstrutor(TypeTuple argsType) {
		for (java.util.Map.Entry<TypeTuple, IEvalueable> entry : this.constructors.entrySet()) {
			TypeTuple type = entry.getKey();
			if(Type.unifyTypes(type, argsType).isPresent()) {
				// If unification exists, return the constructor
				return entry.getValue();
			}				
		}
		return null;
	}
	
	/** Constructs the type */
	public Object construct(TypeTuple argsType, Collection<? extends Object> args, Object env) {
		var ctor = this.findConstrutor(argsType);
		if(ctor == null) {
			throw new RuntimeException("No applicable constructor exists for " + this.type + " with arguments " + argsType);
		}
		
		var o = ctor.evaluate(args, env);
		return o;
	}
	
	/** Adds constructor */
	public void addConstructor(TypeTuple argsType, IEvalueable constructorLambda) {
		if (this.findConstrutor(argsType) != null) {
			throw new RuntimeException("Duplicate constructor of type " + this.type + " with args " + argsType);
		}

		this.constructors.put(argsType, constructorLambda);
	}
	
//	/** Add conversion to specified type atom */
//	public void addConversion(TypeAtom toType, IEvalueable conv, IEvalueable cost) {
//		if (this.conversions.containsKey(toType)) {
//			throw new RuntimeException("Duplicate conversion " + this.type + " to " + toType);
//		}
//		this.conversions.put(toType, new ConversionInfo(this.type, toType, conv, cost));
//	}
//	
//	/** Gers conversion info to given type */
//	protected ConversionInfo getConversion(TypeAtom toType) {
//		return this.conversions.get(toType);
//	}
	
	/** Predicate if TypeAtom can be converted to another */
	public boolean canConvertTo(TypeAtom toType) {
		return toType.representation.equals(TypeRepresentation.WILDCARD)
				|| this.type.representation.equals(TypeRepresentation.WILDCARD)
				|| this.type.name.equals(toType.name);
	}
	
//	/** Converts typa atom */
//	public Object convert(TypeAtom to, Object arg, Object env) {
//		var conv = this.getConversion(to);
//		if(conv == null) {
//			throw new RuntimeException(new StringBuilder("There is no suitable conversion from ")
//					.append(this.type).append(" to ").append(to).toString());
//		}
//		var ret = conv.conversion.evaluate(this.typeSystem.conversionEngine.instantiateCollection(arg), env);
//		return ret;
//	}
	
	@Override
	public String toString() {
		return new StringBuilder()
				.append("TypeInfo: ")
				.append(this.type)
				.toString();
	}
	
	@Override
	public boolean equals(Object other) {
		if(this == other) return true;
		if(other instanceof TypeAtomInfo o) {
			return this.type.equals(o.type);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return this.type.hashCode();
	}
}
