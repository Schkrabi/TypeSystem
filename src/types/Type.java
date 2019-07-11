package types;

import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.stream.Collectors;

import expression.Expression;
import util.AppendableException;

/**
 * Abstract class for types
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public abstract class Type implements Comparable<Type> {
	/**
	 * Type representative
	 */
	//private Type rep;

	public Type() {
		//this.rep = this;
	}

	/**
	 * Returns set of all the variables in the type expression that are not
	 * constrained by quantifier
	 * 
	 * @return
	 */
	public abstract Set<TypeVariable> getUnconstrainedVariables();

	/**
	 * Returns true if this is an applicable type, otherwise returns false
	 * 
	 * @return true or false
	 */
	public boolean isApplicableType() {
		return false;
	}
	
	/**
	 * Returns unified type if the expression if two types unifies, otherwise returns Optional.Empty
	 * @param m first unified type
	 * @param n second unified type
	 * @return Optional<Type>
	 * @throws TypesDoesNotUnifyException
	 */
	public static Optional<Type> unify(Type m, Type n) throws TypesDoesNotUnifyException {
		if(m instanceof TypeConcrete && n instanceof TypeConcrete) {
			TypeConcrete mc = (TypeConcrete)m;
			TypeConcrete nc = (TypeConcrete)n;
			
			if(mc.isSameBasicType(nc)) {
				return Optional.of(mc.baseType());
			}
		} else if(m instanceof TypeArrow && n instanceof TypeArrow) {
			TypeArrow ma = (TypeArrow)m;
			TypeArrow na = (TypeArrow)n;
			
			Optional<Type> left = Type.unify(ma.ltype, na.ltype);
			if(!left.isPresent())
				throw new TypesDoesNotUnifyException(ma.ltype, ma.rtype);
			
			Optional<Type> right = Type.unify(ma.rtype, na.rtype);
			if(!right.isPresent())
				throw new TypesDoesNotUnifyException(ma.rtype, na.rtype);
			
			return Optional.of(new TypeArrow(left.get(), right.get()));
		} else if(m instanceof TypeVariable) {
			return Optional.of(n);
		} else if(n instanceof TypeVariable) {
			return Optional.of(m);
		} else if(m instanceof TypeTuple && n instanceof TypeTuple) {
			TypeTuple mt = (TypeTuple)m;
			TypeTuple nt = (TypeTuple)n;
			
			if(mt.values.length == nt.values.length) {
				Type[] ts = new Type[mt.values.length];
				
				for(int i = 0; i < mt.values.length; i++) {
					Optional<Type> ot = Type.unify(mt.values[i], nt.values[i]);
					
					if(!ot.isPresent())
						throw new TypesDoesNotUnifyException(mt.values[i], nt.values[i]);
					
					ts[i] = ot.get();
				}
				return Optional.of(new TypeTuple(ts));
			}
		}
		
		return Optional.empty();
	}

	/**
	 * Returns true if all types in set unifies
	 * 
	 * @param types unified types
	 * @return true or false
	 */
	public static Optional<Type> unifyMany(Set<Type> types) throws AppendableException {
		if (types.isEmpty())
			return Optional.empty();
		
		// Save type variables for later unification
		Set<Type> notVariables = types.stream().filter(x -> !(x instanceof TypeVariable)).collect(Collectors.toSet());
		
		final Optional<Type> sample = notVariables.stream().findAny();
		if(!sample.isPresent()) {
			return types.stream().findAny();
		}
		
		boolean allTypesEquals = notVariables.stream().map(x -> x.equals(sample.get())).reduce(true, Boolean::logicalAnd);
		if(allTypesEquals) {
			return Optional.of(sample.get());
		}
		
		boolean allTypesConcrete = notVariables.stream().map(x -> x instanceof TypeConcrete).reduce(true, Boolean::logicalAnd);	
		if(allTypesConcrete) {
			boolean unifies = notVariables.stream().map(x -> ((TypeConcrete) x).isSameBasicType((TypeConcrete) sample.get())).reduce(true, Boolean::logicalAnd);
			if(unifies) {
				return Optional.of(((TypeConcrete)sample.get()).baseType());
			}			
		}
		boolean allTypesTypeArrow = notVariables.stream().map(x -> x instanceof TypeArrow).reduce(true, Boolean::logicalAnd);
		if(allTypesTypeArrow) {
			Set<TypeArrow> arrows = notVariables.stream().map(x -> (TypeArrow) x).collect(Collectors.toSet());
			
			Set<Type> lefts = arrows.stream().map(x -> x.ltype).collect(Collectors.toSet());
			Optional<Type> ltype = Type.unifyMany(lefts);
			if(!ltype.isPresent()) {
				throw new TypeSetDoesNotUnifyException(lefts);
			}
			
			Set<Type> rights = arrows.stream().map(x -> x.rtype).collect(Collectors.toSet());
			Optional<Type> rtype = Type.unifyMany(rights);
			if(!rtype.isPresent()) {
				throw new TypeSetDoesNotUnifyException(rights);
			}
			
			Type t = new TypeArrow(ltype.get(), rtype.get());
			
			return Optional.of(t);
		}
		
		boolean allTypesTypeTuple = notVariables.stream().map(x -> x instanceof TypeTuple).reduce(true, Boolean::logicalAnd);
		if(allTypesTypeTuple) {
			boolean allTuplesSameLen = notVariables.stream().map(x -> ((TypeTuple) x).values.length == ((TypeTuple) sample.get()).values.length).reduce(true, Boolean::logicalAnd);
			if(allTuplesSameLen) {
				Type[]  ts = new Type[((TypeTuple)sample.get()).values.length];
				
				for(int i = 0; i < ((TypeTuple)sample.get()).values.length; i++) {
					final int j = i;
					Optional<Type> o = Type.unifyMany(notVariables.stream().map(x -> ((TypeTuple)x).values[j]).collect(Collectors.toSet()));
					if(!o.isPresent()) {
						throw new TypeSetDoesNotUnifyException(notVariables.stream().map(x -> ((TypeTuple)x).values[j]).collect(Collectors.toSet()));
					}
					
					ts[i] = o.get();					
				}
				Type t = new TypeTuple(ts);
				return Optional.of(t);
			}
		}
		
		return Optional.empty();
	}
	
	public static Map<Expression, Type> unionHypothesis(Map<Expression, Type> hyp1, Map<Expression, Type> hyp2) throws AppendableException{
		Set<Expression> unionKeys = hyp1.keySet();
		unionKeys.addAll(hyp2.keySet());
		
		Map<Expression, Type> unionedHypothesis = new TreeMap<Expression, Type>();
		
		while(!unionKeys.isEmpty()) {
			Set<Expression> toRemove = new TreeSet<Expression>();
			
			for(Expression key : unionKeys) {
				Type t1 = hyp1.get(key);
				Type t2 = hyp2.get(key);
				
				if(t1 == null) {
					unionedHypothesis.put(key, t2);
					toRemove.add(key);
				}else if(t2 == null) {
					unionedHypothesis.put(key, t1);
					toRemove.add(key);
				}else if(t1.isAtomicType() && t2.isAtomicType()) {
					Optional<Type> o = Type.unify(t1, t2);
					if(!o.isPresent()) {
						throw new TypesDoesNotUnifyException(t1, t2);
					}
					unionedHypothesis.put(key, o.get());
					toRemove.add(key);
				}else if(t1.isCompositeType() && t2.isCompositeType()) {
					//TODO
				}else {
					
				}
			}
			
			unionKeys.removeAll(toRemove);
		}
		
		return unionedHypothesis;
	}

	@Override
	public int compareTo(Type other) {
		return this.getClass().getName().compareTo(other.getClass().getName());
	}

	/**
	 * Creates expression that converts expr in different type (if possible)
	 * 
	 * @param expr   Expression to be converted
	 * @param toType target type
	 * @return a new expression that will interpret/infer into a targeted type
	 * @throws Exception
	 */
	public abstract Expression convertTo(Expression expr, Type toType) throws Exception;

	/**
	 * Creates expression that converts expr in type consisting only of Basic types
	 * (not specialized type representations)
	 * 
	 * @param expr Expression to be converted
	 * @return a new expression that will intepret/infer into a basic type
	 * @throws Exception
	 */
	public abstract Expression convertToDefaultRepresentation(Expression expr) throws Exception;

	protected void throwConversionError(Expression expr, Type toType) throws Exception {
		throw new Exception("Trying to convert uncovertable types " + this.toString() + " to " + toType.toString()
				+ " on expression " + expr.toString());
	}
	
	/**
	 * Returns true if this type is an atomic type (it is not composed of other types), otherwise returns false
	 * @return true or false
	 */
	public abstract boolean isAtomicType();
	
	/**
	 * Returns true if this type is composite (it is composed of other types), otherwise returns false
	 * @return true or false
	 */
	public boolean isCompositeType() {
		return !this.isAtomicType();
	}
}
