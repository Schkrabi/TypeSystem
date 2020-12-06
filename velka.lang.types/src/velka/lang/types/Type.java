package velka.lang.types;

import java.util.Collection;
import java.util.Set;

import velka.lang.util.AppendableException;
import velka.lang.util.NameGenerator;

/**
 * Abstract class for types
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public abstract class Type implements Comparable<Type> {

	/**
	 * Returns set of all the variables in the type expression that are not
	 * constrained by quantifier
	 * 
	 * @return
	 */
	public abstract Set<TypeVariable> getUnconstrainedVariables();

	/**
	 * Applies substitution to this type
	 * 
	 * @param s applied substitution
	 * @return new Type with applied substitution
	 * @throws TypeVariableNotSubstitutedException
	 * @throws AppendableException
	 */
	public abstract Type apply(Substitution s);

	/**
	 * Tries to unify two types treating different type representation as the same type.
	 * 
	 * @param other other type to unify with
	 * @return substitution unifying the types
	 * @throws AppenableException is thrown if types cannot be unified
	 */
	public abstract Substitution unifyTypeWith(Type other) throws AppendableException;
	
	/**
	 * Tries to unify with other type treating different type representation as different types.
	 * @param other other other type to unify with
	 * @return substitution unifying the types
	 * @throws AppendableException is thrown if types cannot be unified
	 */
	public abstract Substitution unifyRepresentationWith(Type other) throws AppendableException;

	/**
	 * Creates representing type as Clojure key
	 * @return
	 * @throws AppendableException
	 */
	public abstract String clojureTypeRepresentation() throws AppendableException;

	@Override
	public int compareTo(Type other) {
		return this.getClass().getName().compareTo(other.getClass().getName());
	}

	/**
	 * Tries to unify two types treating different type representation as the same type.
	 * 
	 * @param m first unified type
	 * @param n second unified type
	 * @return Most General Unifier of given types
	 * @throws AppendableException if types are not unifiable
	 */
	public static Substitution unifyTypes(Type m, Type n) throws AppendableException {
		return m.unifyTypeWith(n);
	}
	
	/**
	 * Tries to unify two types treating different type representation as different types.
	 * 
	 * @param m Type
	 * @param n Type
	 * @return Most General Unifier of given types
	 * @throws AppendableException if types are not unifiable
	 */
	public static Substitution unifyRepresentation(Type m, Type n) throws AppendableException {
		return m.unifyRepresentationWith(n);
	}

	/**
	 * Unifies set of types
	 * 
	 * @param types set to be unified
	 * @return subtitution unifiing the set
	 * @throws AppendableException thrown if any types does not unify
	 */
	public static Substitution unifyMany(Collection<? extends Type> types) throws AppendableException {
		Type base = new TypeVariable(NameGenerator.next());
		Substitution agg = Substitution.EMPTY;
		for (Type t : types) {
			Substitution s = Type.unifyTypes(base, t);
			agg = agg.union(s);
			base = base.apply(agg);
		}
		return agg;
	}

	/**
	 * Returns true if this type is representing expression that can be applicated
	 * (function, lambda, extended function or lambda). Otherwise returns false.
	 * 
	 * @return true or false.
	 */
	public boolean isApplicableType() {
		return false;
	}
	
	public abstract Type uniteRepresentationsWith(Type other) throws AppendableException;
}
