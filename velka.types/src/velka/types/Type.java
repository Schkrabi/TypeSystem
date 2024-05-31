package velka.types;

import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.BiFunction;

import velka.util.ClojureHelper;
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
	 * Returns true if this type contains any type variables, returns false otherwise.
	 * @return true or false.
	 */
	public boolean hasVariables() {
		return !this.getVariables().isEmpty();
	}

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
			
			agg = agg.compose(s.get());
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
	 * Maps and reduces type to a single value
	 * @param <R> Type of value that reduce will return
	 * @param mapFun maps type atoms to specified value for reduction
	 * @param combiner combines two results
	 * @param terminator initial value to combine with
	 * @return an R value
	 * @throws AppendableException
	 */
	public abstract <R> R reduce(
			java.util.function.Function<TerminalType, R> mapFun, 
			java.util.function.BinaryOperator<R> combiner,
			R terminator) throws AppendableException;
	
	protected abstract <R> R doMap2AndReduce(
			Type other,
			java.util.function.BiFunction<Type, Type, R> mapFun,
			java.util.function.BinaryOperator<R> combinator,
			R terminator) throws AppendableException;
	
	public <R> R map2AndReduce(
			Type other,
			java.util.function.BiFunction<Type, Type, R> mapFun,
			java.util.function.BinaryOperator<R> combinator,
			R terminator) throws AppendableException {
		if(other instanceof RepresentationOr) {
			return other.map2AndReduce(this, mapFun, combinator, terminator);
		}
		
		return this.doMap2AndReduce(other, mapFun, combinator, terminator);
	}
	
	/**
	 * Indicates whether this type can be converted to other
	 * @param other other type
	 * @param atomCheck binary function that checks if type atoms can be converted to one another
	 * @return true if this type can be converted to other, false otherwise
	 */
	public boolean canConvertTo(Type other, BiFunction<TypeAtom, TypeAtom, Boolean> atomCheck) {
		return canConvert(this, other, atomCheck);
	}
	
	/** Checks if from type can be converted to to type */
	public static boolean canConvert(Type from, Type to, BiFunction<TypeAtom, TypeAtom, Boolean> atomCheck) {
		if(from == to
			|| from.equals(to)
			|| to instanceof TypeVariable) {
			return true;
		}
		
		if(to instanceof RepresentationOr) {
			for(Type t : ((RepresentationOr)to).getRepresentations()) {
				if(from.canConvertTo(t, atomCheck)) {
					return true;
				}
			}
			return false;
		}
		
		return from.doCanConvertTo(to, atomCheck);
	}
	
	/**
	 * Worker method for canConvertTo for subclasses
	 * @param other other type
	 * @param atomCheck binary function that checks if type atoms can be converted to one another
	 * @return true if this type can be converted to other, false otherwise
	 */
	public abstract boolean doCanConvertTo(Type other, BiFunction<TypeAtom, TypeAtom, Boolean> atomCheck);
	
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
	
	/**
	 * Removes all representation information from type.
	 * @return Type with representation information removed and replaced with wildcards
	 * @throws AppendableException unlikely
	 */
	public Type removeRepresentationInformation() throws AppendableException {
		return this.map(t -> t instanceof TypeAtom ? new TypeAtom(((TypeAtom)t).name, TypeRepresentation.WILDCARD) : t);
	}

	/**
	 * Adds type meta information to given clojure code piece
	 * 
	 * @param cljCode code to put the meta information to
	 * @param type    type
	 * @return clojure code with meta information
	 */
	public static String addTypeMetaInfo(String cljCode, Type type) {
		try {
			return ClojureHelper.addTypeMetaInfo_str(cljCode, type.clojureTypeRepresentation());
		} catch (AppendableException e) {
			e.printStackTrace();
		}
		return "";
	}
}
