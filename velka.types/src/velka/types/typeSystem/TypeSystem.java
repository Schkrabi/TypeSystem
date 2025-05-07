package velka.types.typeSystem;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import velka.types.RepresentationOr;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeRepresentation;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.util.RankAggregation;
import velka.util.IConversionRanker;
import velka.util.IEvalueable;

/** Type system */
public abstract class TypeSystem {
	protected final Map<TypeAtom, TypeAtomInfo> typeInfo = new HashMap<TypeAtom, TypeAtomInfo>();
	protected final IConversionEngine conversionEngine;
	
	record ConversionKey(TypeAtom from, TypeAtom to) {}
	
	private Map<ConversionKey, IEvalueable> conversionMap = new HashMap<>();
	private Map<ConversionKey, IConversionRanker> conversionCostMap = new HashMap<>();
	
	private RankAggregation agg = RankAggregation.instance();
	
	public TypeSystem(IConversionEngine conversionEngine) {
		this.conversionEngine = conversionEngine;
	}
	
	/** Gets all types that were declared */
	public Collection<TypeAtom> getTypes(){
		return this.typeInfo.keySet();
	}
	
	/** Gets or creates a type info */
	public TypeAtomInfo getOrCreateTypeInfo(TypeAtom type) {
		var ti = typeInfo.get(type);
		if(ti == null) {
			ti = new TypeAtomInfo(type, this);
			this.typeInfo.put(type, ti);
		}
		return ti;
	}
	
	/** Constructs given type */
	public Object construct(TypeAtom type, TypeTuple argsType, Collection<? extends Object> args, Object env) {
		var ti = this.getOrCreateTypeInfo(type);
		var o = ti.construct(argsType, args, env);
		return o;
	}
	
	/** Adds constructor to a type */
	public void addConstructor(TypeAtom type, TypeTuple argsType, IEvalueable ctor) {
		var info = this.getOrCreateTypeInfo(type);
		info.addConstructor(argsType, ctor);
	}
	
	/** Adds new conversion to the type system */
	public void addConversion(TypeAtom fromType, TypeAtom toType, IEvalueable conv, IConversionRanker cost) {
		if (!TypeAtom.isSameBasicType(fromType, toType)) {
			throw new RuntimeException("Can only define conversions between representations!");
		}

		this.conversionMap.put(new ConversionKey(fromType, toType), conv);
		this.conversionCostMap.put(new ConversionKey(fromType, toType), cost);
	}
	
	/** Returns true if from type is convertable to to type. Otherwise returns false */
	public boolean canConvertAtom(TypeAtom from, TypeAtom to) {
		if(from.equals(to)) return true;
		return  to.representation.equals(TypeRepresentation.WILDCARD)
				|| from.representation.equals(TypeRepresentation.WILDCARD)
				|| from.name.equals(to.name);
	}
	
	/** Returns true if first type is converable to the second */
	public boolean canConvert(Type from, Type to) {
		if(from == null || to == null) return false;
		else if(from == to) return true;
		else if((from instanceof TypeVariable) || (to instanceof TypeVariable)) return true;
		else if((from instanceof TypeAtom f) && (to instanceof TypeAtom t)) {
			return this.canConvertAtom(f, t);
		}
		else if((from instanceof TypeTuple f) && (to instanceof TypeTuple t)) {
			if(f.size() != t.size()) return false;
			var i = f.iterator();
			var j = t.iterator();
			while(i.hasNext()) {
				if(!this.canConvert(i.next(), j.next())) return false;
			}
			return true;
		}
		else if((from instanceof TypeArrow f) && (to instanceof TypeArrow t)) {
			return this.canConvert(f.ltype, t.ltype) 
					&& this.canConvert(f.rtype, t.rtype);
		}
		else if(to instanceof RepresentationOr t) {
			var j = t.iterator();
			while(j.hasNext()) {
				if(this.canConvert(from, j.next())) return true;
			}
		}
		else if((from instanceof RepresentationOr f)) {
			var i = f.iterator();
			while(i.hasNext()) {
				var fr = i.next();
				if(this.canConvert(fr, to)) return true;
			}
			
		}
		return false;
	}
	
	public Double conversionCost(Type from, Type to, Object e, Object env) {
		return this.conversionCost(from, to, e, env, agg.worstRank());
	}
	
	/** Gets the cost of representation conversion */
	public double conversionCost(Type from, Type to, Object e, Object env, double bestRank) {		
	    if (from == to || from.equals(to) || to instanceof TypeVariable
	            || from instanceof RepresentationOr
	            || to instanceof RepresentationOr) {
	        return agg.neutralRank();
	    }

	    if (from instanceof TypeArrow) {
	        return agg.functionConversionRank();
	    }

	    if (from instanceof TypeAtom ta && to instanceof TypeAtom toTa) {
	        // wildcard representation shortcut
	        if (TypeRepresentation.WILDCARD.equals(ta.representation) 
	        		|| TypeRepresentation.WILDCARD.equals(toTa.representation)) {
	            return agg.neutralRank();
	        }

	        var costFun = this.conversionCostMap.get(new ConversionKey(ta, toTa));
	        
	        if(costFun == null) return agg.worstRank();

	        var evaluatedCost = costFun.eval(e);

	        return evaluatedCost;
	    }

	    if (from instanceof TypeTuple ftpl && to instanceof TypeTuple ttpl) {
	        if (!(e instanceof Iterable<?> itrbl)) {
	            throw new RuntimeException("Converting non-iterable object with tuple type: " + e);
	        }

	        Iterator<?> ite = itrbl.iterator();
	        double sum = agg.neutralRank();
	        Object te;
	        double cost;

	        for (int i = 0; i < ftpl.size(); i++) {
	            te = ite.next();
	            cost = this.conversionCost(ftpl.get(i), ttpl.get(i), te, env, bestRank);
	            if (cost == agg.invalidRank()) return agg.invalidRank();

	            sum *= cost;
	            
	            //Prune 
	            if(sum <= bestRank) {
	            	return agg.worstRank();
	            }
	        }

	        return sum;
	    }

	    throw new RuntimeException("Invalid conversion cost: unrecognized type: " + from + " or " + to);
	}
	
	/** Converts type atom */
	public Object convertAtom(TypeAtom from, TypeAtom to, Object arg, Object env) {
		if (from.equals(to) || (from.name.equals(to.name) && (from.representation.equals(TypeRepresentation.WILDCARD)
				|| to.representation.equals(TypeRepresentation.WILDCARD)))) {
			return arg;
		}
		if(!this.canConvertAtom(from, to)) {
			throw new RuntimeException(
					new StringBuilder("Cannot convert ")
						.append(from != null ? from.toString() : "nil")
						.append(" to ")
						.append(to != null ? to.toString() : "nil")
						.toString());
		}
//		var ti = this.getOrCreateTypeInfo(from);
//		var ret = ti.convert(to, arg, env);
		var convFun = this.conversionMap.get(new ConversionKey(from, to));
		
		if(convFun == null) {
			throw new RuntimeException(new StringBuilder("There is no suitable conversion from ")
					.append(from).append(" to ").append(to).toString());
		}
		return convFun.evaluate(this.conversionEngine.instantiateCollection(arg), env);
	}
	
	/** Convert types */
	public Object convert(Type from, Type to, Object arg, Object env) {
		if(from == to) {
			return arg;
		}
		
		if(from instanceof TypeVariable) {
			throw new RuntimeException(new StringBuilder("Cannot be type variable")
					.toString());
		}
		if(to instanceof TypeVariable) {
			return arg;
		}
		
		if (from instanceof RepresentationOr 
				|| to instanceof RepresentationOr) {
			if(this.canConvert(from, to)) return arg;
			throw new RuntimeException("Cannot convert");
		}
		
		if(from instanceof TypeTuple ftt) {
			var ttt = (TypeTuple)to;
			return this.conversionEngine.convertTuple(this, ftt, ttt, arg, env);
		}
		if(from instanceof TypeArrow fta) {
			var tta = (TypeArrow)to;
			return this.conversionEngine.convertFunction(this, fta, tta, arg, env);
		}
		if(from instanceof TypeAtom fta) {
			var tta = (TypeAtom)to;
			if(tta.representation.equals(TypeRepresentation.WILDCARD)) {
				return arg;
			}
			
			return this.convertAtom(fta, tta, arg, env);
		}
		throw new RuntimeException(new StringBuilder()
				.append("Unknown conversion error, args: ")
				.append(from != null ? from.toString() : "nil")
				.append(" ")
				.append(to != null ? to.toString() : "nil")
				.append(" ")
				.append(arg != null ? arg.toString() : "nil")
				.append(" ")
				.append(env != null ? env.toString() : "nil")
				.toString());
	}
	
	/** Gets type of an object */
	public abstract Type getType(Object object);
	/** Gets implementation selector of this type system*/
	public abstract ImplementationSelector getImplementationSelector();
}
