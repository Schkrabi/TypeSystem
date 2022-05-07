package velka.types;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Function;
import java.util.stream.Collectors;

import velka.types.RepresentationOr;
import velka.util.AppendableException;
import velka.util.Pair;
import velka.util.ThrowingFunction;

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

		Optional<Substitution> opt = Type.unifyMany(representations);
		if(opt.isEmpty()) {
			throw new TypeSetDoesNotUnifyException(representations);
		}
		
		final Substitution fagg = opt.get();

		Set<Type> unifiedTypes = representations.stream().map(x -> x.apply(fagg)).collect(Collectors.toSet());
		if (unifiedTypes.size() == 1) {
			return unifiedTypes.stream().findAny().get();
		}

		return new RepresentationOr(unifiedTypes);
	}

	/**
	 * Creates new RepresentationOr type. Convenience constructor
	 * 
	 * @param representations types in representation Or
	 * @return new RepresentationOr type
	 * @throws AppendableException thrown if representation is empty or if any of
	 *                             the types in representation does not unify
	 */
	public static Type makeRepresentationOr(Type... reps) throws AppendableException {
		return RepresentationOr.makeRepresentationOr(Arrays.asList(reps));
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
	public Set<TypeVariable> getVariables() {
		final Set<TypeVariable> s = new TreeSet<TypeVariable>();
		representations.stream().forEach(x -> s.addAll(x.getVariables()));
		return s;
	}

	@Override
	public Type apply(Substitution s) {
		return new RepresentationOr(this.representations.stream().map(x -> x.apply(s)).collect(Collectors.toList()));
	}

	/**
	 * Unites list of unifiers
	 * 
	 * @param unifiers list of unifiers
	 * @return substitution uniting all the unifiers
	 * @throws AppendableException if unifiers in list are not compatible
	 */
	private static Optional<Substitution> uniteUnifiers(List<Substitution> unifiers) {
		//In case there are variables substituted in multiple unifiers
		// the substituted reprs must unify on type level
		// then we want to create representationOr from them and use it as substituted representatiation
		// e.g. uniteUnifiers({s\Int:Native}, {s\Int:String}) we want {s\{Int:Native, Int:String}} as a result
		// therefore we cannot use siple reduce
		final Set<TypeVariable> boundVariables = unifiers.stream()
				.map(x -> x.variableStream().collect(Collectors.toSet()))
				.reduce(unifiers.stream().findAny().get().variableStream().collect(Collectors.toSet()), (x, y) -> {
					Set<TypeVariable> s = new TreeSet<TypeVariable>(x);
					s.retainAll(y);
					return s;
				});
		Set<Pair<TypeVariable, Type>> s = new HashSet<Pair<TypeVariable, Type>>();
		for (TypeVariable v : boundVariables) {
			Set<Type> substituted = unifiers.stream().map(x -> x.get(v).get()).collect(Collectors.toSet());
			Optional<Substitution> varUnifier = Type.unifyMany(substituted);
			
			if(varUnifier.isEmpty()) {
				return Optional.empty();
			}
			
			Set<Type> reps = substituted.stream().map(r -> r.apply(varUnifier.get())).collect(Collectors.toSet());
						
			Type t;
			if(reps.size() == 1) {
				t = reps.stream().findAny().get();
			} else {
				t = new RepresentationOr(reps);
			}
						
			Pair<TypeVariable, Type> p = new Pair<TypeVariable, Type>(v, t);
			s.add(p);
		}
		return Optional.of(new Substitution(s));
	}

	@Override
	public Optional<Substitution> unifyTypeWith(Type other) {
		if (other instanceof TypeVariable) {
			return other.unifyTypeWith(this);
		}

		List<Substitution> l = new LinkedList<Substitution>();
		for(Type t : this.representations) {
			Optional<Substitution> opt = Type.unifyTypes(t, other);
			if(opt.isEmpty()) {
				return Optional.empty();
			}
			l.add(opt.get());
		}
		
		Optional<Substitution> united = RepresentationOr.uniteUnifiers(l);  
		return united;
	}

	@Override
	public Optional<Substitution> unifyRepresentationWith(Type other) {
		if (other instanceof TypeVariable) {
			return other.unifyRepresentationWith(this);
		}

		List<Substitution> l = new LinkedList<Substitution>();
		for(Type t : this.representations) {
			Optional<Substitution> opt = Type.unifyRepresentation(t, other);
			if(opt.isEmpty()) {
				return Optional.empty();
			}
			l.add(opt.get());
		}
		
		Optional<Substitution> united = RepresentationOr.uniteUnifiers(l);  
		return united;
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
	public String clojureTypeRepresentation() throws AppendableException {
		StringBuilder sb = new StringBuilder("(velka.types.RepresentationOr/makeRepresentationOr #{");
		Iterator<Type> i = this.representations.iterator();
		while (i.hasNext()) {
			Type t = i.next();
			sb.append(t.clojureTypeRepresentation());
			if (i.hasNext()) {
				sb.append(" ");
			}
		}
		sb.append("})");
		return sb.toString();
	}

	/**
	 * Returns true if this type is representing expression that can be applicated
	 * (function, lambda, extended function or lambda). Otherwise returns false.
	 * 
	 * @return true or false.
	 */
	public boolean isApplicableType() {
		return this.representations.stream().map(x -> x.isApplicableType()).reduce(true, Boolean::logicalAnd);
	}

	@Override
	public Type uniteRepresentationsWith(Type other) throws AppendableException {
		if (other instanceof TypeVariable) {
			return other.uniteRepresentationsWith(this);
		}
		if (other instanceof RepresentationOr) {
			Set<Type> reps = this.getRepresentations();
			reps.addAll(((RepresentationOr) other).getRepresentations());
			return RepresentationOr.makeRepresentationOr(reps);
		}

		if (this.representations.contains(other)) {
			return other;
		}
		throw new AppendableException("Cannot unite types " + this + " " + other);
	}

	@Override
	public Type map(Function<Type, Type> fun) throws AppendableException {
		return RepresentationOr
				.makeRepresentationOr(this.representations.stream().map(fun).collect(Collectors.toList()));
	}

	@Override
	public Type replaceVariable(TypeVariable replaced, TypeVariable replacee) throws AppendableException {
		try {
			return RepresentationOr.makeRepresentationOr(this.representations.stream()
					.map(ThrowingFunction.wrapper(r -> r.replaceVariable(replaced, replacee)))
					.collect(Collectors.toSet()));
		} catch (RuntimeException re) {
			if (re.getCause() instanceof AppendableException) {
				AppendableException e = (AppendableException) re.getCause();
				throw e;
			}
			throw re;
		}
	}
}
