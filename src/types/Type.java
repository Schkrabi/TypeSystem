package types;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
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
	// private Type rep;

	public Type() {
		// this.rep = this;
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
	 * Applies substitution to this type
	 * 
	 * @param s applied substitution
	 * @return new Type with applied substitution
	 */
	public abstract Type apply(Substitution s);

	/**
	 * Returns unified type if the expression if two types unifies, otherwise
	 * returns Optional.Empty
	 * 
	 * @param m first unified type
	 * @param n second unified type
	 * @return Optional<Type>
	 * @throws TypesDoesNotUnifyException
	 */
	public static Optional<Substitution> unify(Type m, Type n) throws TypesDoesNotUnifyException {
		if (m instanceof TypeConcrete && n instanceof TypeConcrete) {
			TypeConcrete mc = (TypeConcrete) m;
			TypeConcrete nc = (TypeConcrete) n;

			if (mc.isSameBasicType(nc)) {
				return Optional.of(new Substitution());
			}
		} else if (m instanceof TypeArrow && n instanceof TypeArrow) {
			TypeArrow ma = (TypeArrow) m;
			TypeArrow na = (TypeArrow) n;

			Optional<Substitution> left = Type.unify(ma.ltype, na.ltype);
			if (!left.isPresent())
				throw new TypesDoesNotUnifyException(ma.ltype, ma.rtype);

			Optional<Substitution> right = Type.unify(ma.rtype, na.rtype);
			if (!right.isPresent())
				throw new TypesDoesNotUnifyException(ma.rtype, na.rtype);

			return Optional.of(left.get().compose(right.get()));
		} else if (m instanceof TypeVariable) {
			Substitution s = new Substitution();
			s.put((TypeVariable) m, n);
			return Optional.of(s);
		} else if (n instanceof TypeVariable) {
			Substitution s = new Substitution();
			s.put((TypeVariable) n, m);
			return Optional.of(s);
		} else if (m instanceof TypeTuple && n instanceof TypeTuple) {
			TypeTuple mt = (TypeTuple) m;
			TypeTuple nt = (TypeTuple) n;

			if (mt.size() == nt.size()) {
				Substitution s = new Substitution();

				Iterator<Type> i = mt.iterator();
				Iterator<Type> j = nt.iterator();
				while (i.hasNext() && j.hasNext()) {
					Type t = i.next();
					Type u = j.next();

					Optional<Substitution> ot = Type.unify(t, u);

					if (!ot.isPresent())
						throw new TypesDoesNotUnifyException(t, u);

					s = s.compose(ot.get());
				}
				return Optional.of(s);
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
		if (!sample.isPresent()) {
			return types.stream().findAny();
		}

		boolean allTypesEquals = notVariables.stream().map(x -> x.equals(sample.get())).reduce(true,
				Boolean::logicalAnd);
		if (allTypesEquals) {
			return Optional.of(sample.get());
		}

		boolean allTypesConcrete = notVariables.stream().map(x -> x instanceof TypeConcrete).reduce(true,
				Boolean::logicalAnd);
		if (allTypesConcrete) {
			boolean unifies = notVariables.stream()
					.map(x -> ((TypeConcrete) x).isSameBasicType((TypeConcrete) sample.get()))
					.reduce(true, Boolean::logicalAnd);
			if (unifies) {
				return Optional.of(((TypeConcrete) sample.get()).baseType());
			}
		}
		boolean allTypesTypeArrow = notVariables.stream().map(x -> x instanceof TypeArrow).reduce(true,
				Boolean::logicalAnd);
		if (allTypesTypeArrow) {
			Set<TypeArrow> arrows = notVariables.stream().map(x -> (TypeArrow) x).collect(Collectors.toSet());

			Set<Type> lefts = arrows.stream().map(x -> x.ltype).collect(Collectors.toSet());
			Optional<Type> ltype = Type.unifyMany(lefts);
			if (!ltype.isPresent()) {
				throw new TypeSetDoesNotUnifyException(lefts);
			}

			Set<Type> rights = arrows.stream().map(x -> x.rtype).collect(Collectors.toSet());
			Optional<Type> rtype = Type.unifyMany(rights);
			if (!rtype.isPresent()) {
				throw new TypeSetDoesNotUnifyException(rights);
			}

			Type t = new TypeArrow(ltype.get(), rtype.get());

			return Optional.of(t);
		}

		boolean allTypesTypeTuple = notVariables.stream().map(x -> x instanceof TypeTuple).reduce(true,
				Boolean::logicalAnd);
		if (allTypesTypeTuple) {
			boolean allTuplesSameLen = notVariables.stream()
					.map(x -> ((TypeTuple) x).size() == ((TypeTuple) sample.get()).size())
					.reduce(true, Boolean::logicalAnd);
			if (allTuplesSameLen) {
				List<Type> ts = new LinkedList<Type>();

				for (int i = 0; i < ((TypeTuple) sample.get()).size(); i++) {
					final int j = i;
					Optional<Type> o = Type.unifyMany(
							notVariables.stream().map(x -> ((TypeTuple) x).get(j)).collect(Collectors.toSet()));
					if (!o.isPresent()) {
						throw new TypeSetDoesNotUnifyException(
								notVariables.stream().map(x -> ((TypeTuple) x).get(j)).collect(Collectors.toSet()));
					}

					ts.add(o.get());
				}
				Type t = new TypeTuple(ts);
				return Optional.of(t);
			}
		}

		return Optional.empty();
	}

	public static Map<Expression, Type> unionHypothesis(Map<Expression, Type> hyp1, Map<Expression, Type> hyp2)
			throws AppendableException {
		throw new AppendableException("Not implemented");
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
	public abstract Expression convertTo(Expression expr, Type toType) throws AppendableException;

	//TODO create new exception
	protected void throwConversionError(Expression expr, Type toType) throws AppendableException {
		throw new AppendableException("Trying to convert uncovertable types " + this.toString() + " to " + toType.toString()
				+ " on expression " + expr.toString());
	}

	/**
	 * Returns true if this type is an atomic type (it is not composed of other
	 * types), otherwise returns false
	 * 
	 * @return true or false
	 */
	public abstract boolean isAtomicType();

	/**
	 * Returns true if this type is composite (it is composed of other types),
	 * otherwise returns false
	 * 
	 * @return true or false
	 */
	public boolean isCompositeType() {
		return !this.isAtomicType();
	}
}
