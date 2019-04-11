package types;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.Arrays;

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

	/**
	 * Ordering of subclasses for Comparable interface
	 */
	private static List<Class<? extends Type>> ordering = Arrays.asList(TypeConcrete.class, TypeRepresentation.class,
			TypeVariable.class, ForallType.class, TypeArrow.class, TypeTuple.class);

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
		if(m == n) {
			return Optional.of(m);
		} else if(m instanceof TypeConcrete && n instanceof TypeConcrete) {
			TypeConcrete mc = (TypeConcrete)m;
			TypeConcrete nc = (TypeConcrete)n;
			
			if(mc.isSameBasicType(nc)) {
				return Optional.of(mc.getBasicType());
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
			
			return Optional.of(new TypeArrow(left.get(), right.get()).quantifyUnconstrainedVariables());
		} else if(m instanceof TypeVariable) {
			return Optional.of(n.quantifyUnconstrainedVariables());
		} else if(n instanceof TypeVariable) {
			return Optional.of(m.quantifyUnconstrainedVariables());
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
				return Optional.of(new TypeTuple(ts).quantifyUnconstrainedVariables());
			}
		} else if(m instanceof ForallType) {
			ForallType mf = (ForallType)m;
			return Type.unify(mf.getBoundType(), n);
		}else if (n instanceof ForallType) {
			ForallType nf = (ForallType)n;
			return Type.unify(m, nf.getBoundType());
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
		
		Set<Type> unbound = types.stream().map(x -> (x instanceof ForallType) ? ((ForallType)x).getBoundType() : x).collect(Collectors.toSet());
		// Save type variables for later unification
		unbound = unbound.stream().filter(x -> !(x instanceof TypeVariable)).collect(Collectors.toSet());
		
		final Optional<Type> sample = unbound.stream().findAny();
		if(!sample.isPresent()) {
			return types.stream().findAny();
		}
		
		boolean allTypesEquals = unbound.stream().map(x -> x.equals(sample.get())).reduce(true, Boolean::logicalAnd);
		if(allTypesEquals) {
			return Optional.of(sample.get());
		}
		
		boolean allTypesConcrete = unbound.stream().map(x -> x instanceof TypeConcrete).reduce(true, Boolean::logicalAnd);	
		if(allTypesConcrete) {
			boolean unifies = unbound.stream().map(x -> ((TypeConcrete) x).isSameBasicType((TypeConcrete) sample.get())).reduce(true, Boolean::logicalAnd);
			if(unifies) {
				return Optional.of(((TypeConcrete)sample.get()).baseType());
			}			
		}
		boolean allTypesTypeArrow = unbound.stream().map(x -> x instanceof TypeArrow).reduce(true, Boolean::logicalAnd);
		if(allTypesTypeArrow) {
			Set<TypeArrow> arrows = unbound.stream().map(x -> (TypeArrow) x).collect(Collectors.toSet());
			
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
			
			return Optional.of(t.quantifyUnconstrainedVariables());
		}
		
		boolean allTypesTypeTuple = unbound.stream().map(x -> x instanceof TypeTuple).reduce(true, Boolean::logicalAnd);
		if(allTypesTypeTuple) {
			boolean allTuplesSameLen = unbound.stream().map(x -> ((TypeTuple) x).values.length == ((TypeTuple) sample.get()).values.length).reduce(true, Boolean::logicalAnd);
			if(allTuplesSameLen) {
				Type[]  ts = new Type[((TypeTuple)sample.get()).values.length];
				
				for(int i = 0; i < ((TypeTuple)sample.get()).values.length; i++) {
					final int j = i;
					Optional<Type> o = Type.unifyMany(unbound.stream().map(x -> ((TypeTuple)x).values[j]).collect(Collectors.toSet()));
					if(!o.isPresent()) {
						throw new TypeSetDoesNotUnifyException(unbound.stream().map(x -> ((TypeTuple)x).values[j]).collect(Collectors.toSet()));
					}
					
					ts[i] = o.get();					
				}
				Type t = new TypeTuple(ts);
				return Optional.of(t.quantifyUnconstrainedVariables());
			}
		}
		
		return Optional.empty();
	}

	@Override
	public int compareTo(Type other) {
		return (int) Math.signum(ordering.indexOf(this.getClass()) - ordering.indexOf(other.getClass()));
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
	 * Adds universal quantifies for all unconstrained variables of this type
	 * @return identic type with all type variables quantified
	 */
	public Type quantifyUnconstrainedVariables() {
		Type t = this;
		for(TypeVariable tv : this.getUnconstrainedVariables()) {
			t = new ForallType(tv, t);
		}
		return t;
	}
}
