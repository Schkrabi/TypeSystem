package types;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import expression.Expression;
import util.AppendableException;
import util.Pair;
import util.ThrowingFunction;

/**
 * This class represents set of unified types that differs only in their low
 * level representation
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class RepresentationOr extends Type {

	/**
	 * All types this Or is representing
	 */
	private Set<Type> representations;

	/**
	 * Creates new RepresentationOr type
	 * 
	 * @param representations types in representation Or
	 * @return new RepresentationOr type
	 * @throws AppendableException thrown if representation is empty or if any of
	 *                             the types in representation does not unify
	 */
	public static Type makeRepresentationOr(Collection<? extends Type> representations) throws AppendableException {
		if (representations.isEmpty()) {
			throw new AppendableException("Cannot make empty RepresentationOr! ");
		}
		if (representations.size() == 1) {
			return representations.stream().findAny().get();
		}

		final Substitution fagg = Type.unifyMany(representations);
		
		Set<Type> unifiedTypes = representations.stream().map(x -> x.apply(fagg)).collect(Collectors.toSet());
		if(unifiedTypes.size() == 1) {
			return unifiedTypes.stream().findAny().get();
		}

		return new RepresentationOr(unifiedTypes);
	}

	private RepresentationOr(Collection<Type> representations) {
		this.representations = new TreeSet<Type>(representations);
	}

	/**
	 * Gets the set of representation of this RepresentationOr
	 * 
	 * @return new Set of representations
	 */
	public Set<Type> getRepresentations() {
		return new TreeSet<Type>(this.representations);
	}

	@Override
	public Set<TypeVariable> getUnconstrainedVariables() {
		final Set<TypeVariable> s = new TreeSet<TypeVariable>();
		representations.stream().forEach(x -> s.addAll(x.getUnconstrainedVariables()));
		return s;
	}

	@Override
	public Type apply(Substitution s) {
		return new RepresentationOr(this.representations.stream().map(x -> x.apply(s)).collect(Collectors.toList()));
	}

	@Override
	public Expression convertTo(Expression expr, Type toType) throws AppendableException {
		throw new AppendableException(
				"Cannot directly convert RepresentationOr type. Select specific representation in order to create conversion.");
	}

	@Override
	public String convertToClojure(String argument, Type toType) throws AppendableException {
		throw new AppendableException(
				"Cannot directly convert RepresentationOr type. Select specific representation in order to create conversion.");
	}

	@Override
	public Substitution unifyWith(Type other) throws AppendableException {
		if(other instanceof TypeVariable) {
			return other.unifyWith(this);
		}
		
		try {
			final List<Substitution> unifiers = this.representations.stream()
					.map(ThrowingFunction.wrapper(x -> Type.unify(x, other))).collect(Collectors.toList());

			//This step might be redundant, because all substituted variables in RepresentationOr should be the same for each representation
			final Set<TypeVariable> boundVariables = unifiers.stream().map(x -> x.variableStream().collect(Collectors.toSet()))
					.reduce(unifiers.stream().findAny().get().variableStream().collect(Collectors.toSet()), (x, y) -> {
						Set<TypeVariable> s = new TreeSet<TypeVariable>(x);
						s.retainAll(y);
						return s;
					});
			
			Set<Pair<TypeVariable, Type>> s = new HashSet<Pair<TypeVariable, Type>>();
			for(TypeVariable v : boundVariables) {
				Type t = RepresentationOr.makeRepresentationOr(unifiers.stream().map(x -> x.get(v).get()).collect(Collectors.toSet()));
				Pair<TypeVariable, Type> p = new Pair<TypeVariable, Type>(v, t);
				s.add(p);
			}
			return new Substitution(s);
			
		} catch (RuntimeException e) {
			AppendableException ae = (AppendableException) e.getCause();
			throw ae;
		}
	}

	@Override
	public Type removeRepresentationInfo() {
		return this;
	}

	@Override
	public int compareTo(Type other) {
		if (!(other instanceof RepresentationOr)) {
			return super.compareTo(other);
		}
		RepresentationOr o = (RepresentationOr) other;
		if (this.representations.size() != o.representations.size()) {
			return this.representations.size() - o.representations.size();
		}

		for (Type t : this.representations) {
			int cmp = o.representations.stream().map(x -> Math.abs(x.compareTo(t))).reduce(Integer.MAX_VALUE,
					Math::min);
			if (cmp != 0) {
				return cmp;
			}
		}

		for (Type t : o.representations) {
			int cmp = -this.representations.stream().map(x -> Math.abs(x.compareTo(t))).reduce(Integer.MAX_VALUE,
					Math::min);
			if (cmp != 0) {
				return cmp;
			}
		}

		return 0;
	}

	@Override
	public boolean equals(Object other) {
		if (!(other instanceof RepresentationOr)) {
			return false;
		}
		return this.representations.equals(((RepresentationOr) other).representations);
	}

	@Override
	public String toString() {
		return this.representations.toString();
	}
	
	@Override
	public int hashCode() {
		return this.representations.hashCode();
	}

	@Override
	public String toClojure() throws AppendableException{
		throw new AppendableException("toClojure of " + this.getClass().getName() + " : " + this + " is not allowed");
	}
}
