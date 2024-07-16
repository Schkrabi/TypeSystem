package velka.types.typeSystem;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import velka.types.RepresentationOr;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeRepresentation;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.util.CostAggregation;

/** Type system */
public class TypeSystem {
	private final Map<TypeAtom, TypeAtomInfo> typeInfo = new HashMap<TypeAtom, TypeAtomInfo>();
	private final IConversionEngine conversionEngine;
	
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
			ti = new TypeAtomInfo(type);
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
	public void addConversion(TypeAtom fromType, TypeAtom toType, IEvalueable conv, IEvalueable cost) {
		if (!TypeAtom.isSameBasicType(fromType, toType)) {
			throw new RuntimeException("Can only define conversions between representations!");
		}

		var info = this.getOrCreateTypeInfo(fromType);
		info.addConversion(toType, conv, cost);
	}
	
	/** Returns true if from type is convertable to to type. Otherwise returns false */
	public boolean canConvertAtom(TypeAtom from, TypeAtom to) {
		var info = this.getOrCreateTypeInfo(from);
		return info.canConvertTo(to);
	}
	
	/** Returns true if first type is converable to the second */
	public boolean canConvert(Type from, Type to) {
		if(from == null || to == null) return false;
		else if(from == to || from.equals(to)) return true;
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
	
	/** Gets the cost of representation conversion */
	public Double conversionCost(Type from, Type to, Object e, Object env) {
		if(from.equals(to)) {
			return CostAggregation.instance().neutralRank();
		}
		else if(!this.canConvert(from, to)) {
			return null;
		}
		else if(to instanceof TypeVariable
				|| from instanceof RepresentationOr
				|| to instanceof RepresentationOr) {
			return CostAggregation.instance().neutralRank();
		}
		else if(from instanceof TypeAtom ta) {
			var toTa = (TypeAtom)to;
			if(toTa.representation.equals(TypeRepresentation.WILDCARD)) {
				return CostAggregation.instance().neutralRank();
			}
			
			var ti = this.getOrCreateTypeInfo(ta);
			var ci = ti.getConversion((TypeAtom)to);
			
			if(ci == null) return null;		
			
			var c = ci.cost().evaluate(this.conversionEngine.instantiateCollection(e), env);
			
			if(!(c instanceof Double)) {
				throw new RuntimeException("Invalid conversion cost: conversion " + from + " to " + to + " has improper cost function.");
			}
			
			return (Double)c;
		}
		else if(from instanceof TypeTuple) {
			var sum = CostAggregation.instance().neutralRank();
			if(!(e instanceof Iterable)) {
				throw new RuntimeException("Converting not iterable object with type tuple type " + e.toString());
			}
			
			@SuppressWarnings("unchecked")
			var ite = ((Iterable<Object>)e).iterator();
			var itf = ((TypeTuple)from).iterator();
			var itt = ((TypeTuple)to).iterator();
			while(itf.hasNext()) {
				var sef = itf.next();
				var set = itt.next();
				var te = ite.next();
				var cost = this.conversionCost(sef, set, te, env);
				if(cost == null) return null;
				
				sum = CostAggregation.instance().aggregate(sum, cost);
			}
			return sum;
		}
		else if(from instanceof TypeArrow) {
			// The time to convert the function is constant
			// What can change is the execution time of the function
			// However that is not traceable for Velka
			return CostAggregation.instance().functionConversionRank();
		}
		throw new RuntimeException("Invalid conversion cost: unrecognized type: " + from + " or " + to);
	}
	
	/** Converts type atom */
	public Object convertAtom(TypeAtom from, TypeAtom to, Collection<? extends Object> args, Object env) {
		var ti = this.getOrCreateTypeInfo(from);
		var ret = ti.convert(to, args, env);
		return ret;
	}
	
	/** Convert types */
	public Object convert(Type from, Type to, Collection<? extends Object> args, Object env) {
		var arg = args.iterator().next();
		if(from.equals(to)) return arg;
		
		if(!this.canConvert(from, to)) {
			throw new RuntimeException("Cannot convert " + (from != null ? from.toString() : "nil") + " to " + to);
		}
		
		if (from instanceof TypeVariable 
				|| from instanceof RepresentationOr
				|| to instanceof TypeVariable 
				|| to instanceof RepresentationOr)
			return arg;
		
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
			
			return this.convertAtom(fta, tta, args, env);
		}
		throw new RuntimeException(new StringBuilder()
				.append("Unknown conversion error, args: ")
				.append(from)
				.append(" ")
				.append(to)
				.append(" ")
				.append(args)
				.append(" ")
				.append(env)
				.toString());
	}
}
