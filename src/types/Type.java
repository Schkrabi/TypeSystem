package types;

import java.util.Collection;
import java.util.Set;

import expression.Expression;
import util.AppendableException;
import util.NameGenerator;

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
	 * Creates expression that converts expr in different type (if possible)
	 * 
	 * @param expr   Expression to be converted
	 * @param toType target type
	 * @return a new expression that will interpret/infer into a targeted type
	 * @throws Exception
	 */
	public abstract Expression convertTo(Expression expr, Type toType) throws AppendableException;

	/**
	 * Returns substitution that unifies this type with other type
	 * 
	 * @param other other type to unify with
	 * @return substitution unifying the types
	 * @throws TypesDoesNotUnifyException is thrown if types cannot be unified
	 */
	public abstract Substitution unifyWith(Type other) throws AppendableException;

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
	 * Equality based on unification. Returns true if two types unifies, false otherwise.
	 * @param other other type
	 * @return true or false
	 */
	public boolean equalsUpToIsomorphism(Type other) {
		try {
			Type.unify(this, other);
		}catch(AppendableException e) {
			return false;
		}
		return true;
	}

	/**
	 * Returns unified type if the expression if two types unifies, otherwise throws
	 * 
	 * @param m first unified type
	 * @param n second unified type
	 * @return MGU of given types
	 * @throws AppendableException if types are not unifiable
	 */
	public static Substitution unify(Type m, Type n) throws AppendableException {
		return m.unifyWith(n);
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
			Substitution s = Type.unify(base, t);
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
