package types;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.Arrays;

import expression.Expression;

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
	private Type rep;

	/**
	 * Ordering of subclasses for Comparable interface
	 */
	private static List<Class<? extends Type>> ordering = Arrays.asList(TypeConcrete.class, TypeRepresentation.class,
			TypeVariable.class, ForallType.class, TypeArrow.class, TypeTuple.class);

	public Type() {
		this.rep = this;
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
	 * Returns true if two types unifies, otherwise returns false.
	 * 
	 * @warning Might change the type representatives if unification is succesfull
	 * @param m Type
	 * @param n Type
	 * @return true or false
	 */
	public static boolean unify(Type m, Type n) {
		Type s = m.getRep();
		if (s instanceof ForallType) {
			s = ((ForallType) s).getBoundType();
		}
		Type t = n.getRep();
		if (t instanceof ForallType) {
			t = ((ForallType) t).getBoundType();
		}

		if (s == t) {
			return true;
		} else if (t instanceof TypeConcrete && s instanceof TypeConcrete) {
			return ((TypeConcrete) s).isSameBasicType((TypeConcrete) t);
		} else if (s instanceof TypeArrow && t instanceof TypeArrow) {
			Type.union(s, t);
			TypeArrow as = (TypeArrow) s;
			TypeArrow at = (TypeArrow) t;
			return Type.unify(as.ltype, at.ltype) && Type.unify(as.rtype, at.rtype);
		} else if (s instanceof TypeVariable || t instanceof TypeVariable) {
			Type.union(s, t);
			return true;
		} else if (s instanceof TypeTuple && t instanceof TypeTuple) {
			Type.union(s, t);
			TypeTuple ts = (TypeTuple) s;
			TypeTuple tt = (TypeTuple) t;

			if (ts.values.length != tt.values.length) {
				return false;
			}

			for (int i = 0; i < ts.values.length; i++) {
				if (!Type.unify(ts.values[i], tt.values[i])) {
					return false;
				}
			}
			return true;
		}
		return false;
	}

	/**
	 * Returns true if all types in set unifies
	 * 
	 * @param types unified types
	 * @return true or false
	 */
	public static boolean unifyMany(Set<Type> types) {
		if (types.isEmpty())
			return false;

		boolean unifies = false;

		Set<Type> reps = types.stream().map(x -> (x.getRep())).collect(Collectors.toSet());

		Set<Type> unbound = reps.stream().map(x -> x instanceof ForallType ? ((ForallType) x).getBoundType() : x)
				.collect(Collectors.toSet());

		// Save type variables for later unification
		Set<Type> typeVariables = unbound.stream().filter(x -> x instanceof TypeVariable).collect(Collectors.toSet());
		unbound.removeAll(typeVariables);

		final Optional<Type> sample = unbound.stream().findAny();

		boolean allTypesEquals = unbound.stream().map(x -> x.equals(sample.get())).reduce(true, (x, y) -> x && y);

		if (allTypesEquals) {
			unifies = true;
		} // All Types are TypeConcrete
		else if (unbound.stream().map(x -> x instanceof TypeConcrete).reduce(true, (x, y) -> x && y)) {
			unifies = unbound.stream().map(x -> ((TypeConcrete) x).isSameBasicType((TypeConcrete) sample.get())).reduce(true,
					(x, y) -> x && y);
		} // All types are TypeArrows
		else if (unbound.stream().map(x -> x instanceof TypeArrow).reduce(true, (x, y) -> x && y)) {
			Type.unionMany(unbound);
			Set<TypeArrow> arrows = unbound.stream().map(x -> (TypeArrow) x).collect(Collectors.toSet());
			Set<Type> lefts = arrows.stream().map(x -> x.ltype).collect(Collectors.toSet());
			Set<Type> rights = arrows.stream().map(x -> x.rtype).collect(Collectors.toSet());
			unifies = Type.unifyMany(lefts) && Type.unifyMany(rights);
		} // All types are TypeTuples
		else if (unbound.stream().map(x -> x instanceof TypeTuple).reduce(true, (x, y) -> x && y)) {
			// All TypeTuples are of same length
			if (unbound.stream().map(x -> ((TypeTuple) x).values.length == ((TypeTuple) sample.get()).values.length)
					.reduce(true, (x, y) -> x && y)) {
				Type.unionMany(unbound);

				boolean tupleElementsUnifiesSoFar = true;
				for (int i = 0; i < ((TypeTuple) sample.get()).values.length; i++) {
					final int j = i;
					tupleElementsUnifiesSoFar = tupleElementsUnifiesSoFar && Type.unifyMany(
							unbound.stream().map(x -> ((TypeTuple) x).values[j]).collect(Collectors.toSet()));
					if (!tupleElementsUnifiesSoFar) {
						break;
					}
				}
				unifies = tupleElementsUnifiesSoFar;
			}
		}

		// Union variables if non-variables unifies (sample is already unioned)
		if (unifies) {
			if(sample.isPresent())
				typeVariables.add(sample.get());
			Type.unionMany(typeVariables);
		}

		return unifies;
	}

	/**
	 * Gets the representative of this type
	 * 
	 * @return Type
	 */
	public Type getRep() {
		if (this.rep == this) {
			return this.rep;
		}
		return this.rep.getRep();
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
	 * Unions the representatives of two types
	 * 
	 * @param t1 Type
	 * @param t2 Type
	 */
	private static void union(Type t1, Type t2) {
		if (t1 instanceof TypeConcrete) {
			t2.rep = t1.rep;
			return;
		}
		if (t2 instanceof TypeConcrete) {
			t1.rep = t2.rep;
		}
		t2.rep = t1.rep;
		return;
	}

	/**
	 * Unions representation of set of types
	 * 
	 * @param s set of types to be unioned
	 */
	private static void unionMany(Set<Type> s) {
		if (s.isEmpty()) {
			return;
		}
		Type concrete = s.stream().findAny().get();
		Optional<Type> sample = s.stream().filter(x -> x instanceof TypeConcrete).findAny();
		if (sample.isPresent()) {
			concrete = sample.get();
		}
		for (Type t : s.stream().filter(x -> !(x instanceof TypeConcrete)).collect(Collectors.toSet())) {
			t.rep = concrete.rep;
		}
	}
}
