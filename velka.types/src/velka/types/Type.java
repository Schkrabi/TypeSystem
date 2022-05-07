package velka.types;

import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import velka.types.Type;
import velka.util.AppendableException;
import velka.util.NameGenerator;

/**
 * Abstract class for types
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public abstract class Type implements Comparable<Type> {

	/**
	 * Returns set of all the variables in the type 
	 * 
	 * @return
	 */
	public abstract Set<TypeVariable> getVariables();

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
	 * @param other other other type to unify with
	 * @return Optional.Empty if types cannot be unified. Otherwise Optional containing unifier
	 */
	public abstract Optional<Substitution> unifyTypeWith(Type other);
	
	/**
	 * Tries to unify with other type treating different type representation as different types.
	 * @param other other other other type to unify with
	 * @return Optional.Empty if types cannot be unified. Otherwise Optional containing unifier
	 */
	public abstract Optional<Substitution> unifyRepresentationWith(Type other);

	/**
	 * Creates representing type as Clojure key
	 * @return
	 * @throws AppendableException
	 */
	public abstract String clojureTypeRepresentation() throws AppendableException;
	
	/**
	 * Replaces all occurences of variable replaced by variable replacee
	 * @param replaced type variable to be replaced
	 * @param replacee type variable that will be in place of replaced variable
	 * @return type
	 */
	public abstract Type replaceVariable(TypeVariable replaced, TypeVariable replacee) throws AppendableException;
	
	/**
	 * Replaces all occurences of any type variable from key set of replacement map with mapped type variable
	 * 
	 * @param replacementMap map of replaced type variables and by what type variables are they replaced
	 * @return new Type instance
	 * @throws AppendableException
	 */
	public Type replaceVariables(Map<TypeVariable, TypeVariable> replacementMap) throws AppendableException{
		Type t = this;
		for(Map.Entry<TypeVariable, TypeVariable> e : replacementMap.entrySet()) {
			t = t.replaceVariable(e.getKey(), e.getValue());
		}
		return t;
	}
	
	/**
	 * Replaces all occurences of any type variable from key set of replacement map with new unique type variables
	 * 
	 * @param typeVariables set of replace type variables
	 * @return new Type instance
	 * @throws AppendableException
	 */
	public Type replaceVariables(Set<TypeVariable> typeVariables) throws AppendableException {
		Type t = this;
		for(TypeVariable tv : typeVariables) {
			t = t.replaceVariable(tv, new TypeVariable(NameGenerator.next()));
		}
		return t;
	}

	@Override
	public int compareTo(Type other) {
		return this.getClass().getName().compareTo(other.getClass().getName());
	}

	/**
	 * Tries to unify two types treating different type representation as the same type.
	 * @param m first unified type
	 * @param n second unified type
	 * @return Optional.Empty if types cannot be unified. Otherwise Optional containing unifier
	 */
	public static Optional<Substitution> unifyTypes(Type m, Type n){
		return m.unifyTypeWith(n);
	}
	
	/**
	 * Tries to unify two types treating different type representation as different types.
	 * @param m first unified type
	 * @param n second unified type
	 * @return Optional.Empty if types cannot be unified. Otherwise Optional containing unifier
	 */
	public static Optional<Substitution> unifyRepresentation(Type m, Type n){
		return m.unifyRepresentationWith(n);
	}

	/**
	 * 
	 * 
	 * @param 
	 * @return subtitution unifiing the set
	 * @throws AppendableException thrown if any types does not unify
	 */
	/**
	 * Unifies set of types
	 * @param types types set to be unified
	 * @return
	 * @throws AppendableException
	 */
	public static Optional<Substitution> unifyMany(Collection<? extends Type> types) {
		Type base = new TypeVariable(NameGenerator.next());
		Substitution agg = Substitution.EMPTY;
		for (Type t : types) {
			Optional<Substitution> s = Type.unifyTypes(base, t);
			if(s.isEmpty()) {
				return Optional.empty();
			}
			Optional<Substitution> opt = agg.union(s.get()); 
			
			if(opt.isEmpty()) {
				return Optional.empty();
			}
			
			agg = opt.get();
			base = base.apply(agg);
		}
		return Optional.of(agg);
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
	
	/**
	 * Maps function recursively on type
	 * @param fun function
	 * @return type with applied function
	 */
	public abstract Type map(java.util.function.Function<Type, Type> fun) throws AppendableException;
	
	/**
	 * Replaces all type variables with new unused names
	 * 
	 * @param type type where type variable will be replaced
	 * @return type with type variables replaced
	 * @throws AppendableException 
	 */
	public static Type renameAllVariables(Type type) throws AppendableException {
		Set<TypeVariable> vars = type.getVariables();
		Type t = type;
		for(TypeVariable v : vars) {
			t = t.replaceVariable(v, new TypeVariable(NameGenerator.next()));
		}
		return t;
	}
}
