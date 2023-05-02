package velka.types;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import velka.types.Substitution;
import velka.util.AppendableException;
import velka.util.NameGenerator;
import velka.util.Pair;

/**
 * Substituion used for type inference
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Substitution {
	/**
	 * Elements of the substitution
	 */
	private final TreeMap<TypeVariable, Type> elements = new TreeMap<TypeVariable, Type>();

	private Substitution() {
	}
	
	public Substitution(Substitution cloned) {
		this.elements.putAll(cloned.elements);
	}

	public Substitution(Collection<Pair<TypeVariable, Type>> init) {
		init.stream().forEach(x -> this.elements.put(x.first, x.second));
	}

	@SafeVarargs
	public Substitution(Pair<TypeVariable, Type>... pairs) {
		Arrays.asList(pairs).stream().forEach(x -> this.elements.put(x.first, x.second));
	}

	/**
	 * Gets Type which is substituted for given variable. If variable is not
	 * substituted returns Optional.empty()
	 * 
	 * @param v Substituted variable
	 * @return Optional<Type>
	 */
	public Optional<Type> get(TypeVariable v) {
		Type t = this.elements.get(v);
		if (t == null) {
			return Optional.empty();
		}
		return Optional.of(t);
	}
	
	/**
	 * Gets copy of underlying set of pairs
	 * @return set of pairs
	 */
	public Set<Pair<TypeVariable, Type>> asSet(){
		return this.elements.entrySet().stream().map(e -> Pair.of(e.getKey(), e.getValue())).collect(Collectors.toSet());
	}

	/**
	 * Returns true if this substitution contains substitution for given variable.
	 * Otherwise returns false.
	 * 
	 * @param v checked variable
	 * @return true or false
	 */
	public boolean containsVariable(TypeVariable v) {
		return this.elements.containsKey(v);
	}
	
	/**
	 * Substitution composition as is defined in J.W.Lloyd: Logic Programming
	 * @param other other substitution
	 * @return new instance of Substitution that is composed from this and other
	 */
	public Substitution compose(Substitution other) {
		Substitution composed = new Substitution();
		
		for(Map.Entry<TypeVariable, Type> e : this.elements.entrySet()) {
			TypeVariable v = e.getKey();
			Type t = e.getValue().apply(other);
			if(!v.equals(t)) {
				composed.elements.put(v, t);
			}
		}
		
		Set<TypeVariable> excludedVariables = this.elements.keySet();
		for(Map.Entry<TypeVariable, Type> e : other.elements.entrySet()) {
			if(!excludedVariables.contains(e.getKey())) {
				composed.elements.put(e.getKey(), e.getValue());
			}
		}
		
		return composed;
	}
	
	/**
	 * Creates new substitution, where on each right side of an element is applied the other substitution.
	 * @param other substitution
	 * @return new Substitution instance
	 */
	protected Substitution apply(Substitution other) {
		Substitution ret = new Substitution();
		
		for(Map.Entry<TypeVariable, Type> e : this.elements.entrySet()) {
			ret.elements.put(e.getKey(), e.getValue().apply(other));
		}
		
		return ret;
	}
	
	/**
	 * Finds all common variables between this and other, where right sides of the substitutions are not equal
	 * @param other substitution
	 * @return set of TypeVariables
	 */
	protected Set<TypeVariable> conflictingVariables(Substitution other){
		Set<TypeVariable> intersection = new TreeSet<TypeVariable>();
		intersection.addAll(this.elements.keySet());
		intersection.retainAll(other.elements.keySet());

		return intersection.stream()
				.filter(v -> !this.get(v).equals(other.get(v)))
				.filter(v -> { //Have to filter out cases Type:Rep1 x Type:*, which are not equal but not conflicting
					Type t1 = this.get(v).get();
					Type t2 = other.get(v).get();					
					if(t1 instanceof TypeAtom && t2 instanceof TypeAtom) {
						TypeAtom ta1 = (TypeAtom)t1;
						TypeAtom ta2 = (TypeAtom)t2;
						return 	!(	ta1.name.equals(ta2.name)
								 &&	(	ta1.representation.equals(TypeRepresentation.WILDCARD)
									||	ta2.representation.equals(TypeRepresentation.WILDCARD)));
					}
					return true;
				})
				.collect(Collectors.toSet());
	}
	
	/**
	 * Merges this substitution with another to new substitution
	 * @param other substitution
	 * @return Optinal.Empty if substitutions cannot be merged. Otherwise Optional with merged substitution
	 */
	public Optional<Substitution> merge(Substitution other){
		Substitution s1 = new Substitution(this);
		Substitution s2 = new Substitution(other);
		
		Set<TypeVariable> conflicting = s1.conflictingVariables(s2);
		while(!conflicting.isEmpty()) {
			TypeVariable v = conflicting.stream().findAny().get();
			Optional<Substitution> mgu = Type.unifyTypes(s1.elements.get(v), s2.elements.get(v));
			if(mgu.isEmpty()) {
				return Optional.empty();
			}
			s1 = s1.compose(mgu.get());
			s2 = s2.compose(mgu.get());
			conflicting = s1.conflictingVariables(s2);
		}
		
		return Optional.of(s1.compose(s2));
	}

	@Override
	public String toString() {
		return this.elements.toString();
	}

	/**
	 * Gets stream of substituted variables
	 * 
	 * @return stream object
	 */
	public Stream<TypeVariable> variableStream() {
		return this.elements.keySet().stream();
	}
	
	/**
	 * Get stream of TypeVariable - Type pair
	 * 
	 * @return stream of pairs
	 */
	public Stream<Pair<TypeVariable, Type>> stream(){
		return this.elements.entrySet().stream().map(e -> new Pair<TypeVariable, Type>(e.getKey(), e.getValue()));
	}
	
	/**
	 * Removes typevariable from all lefts sides of subsitutions and replaces it with replacee variable on right sides of remaining substituions
	 * 
	 * @param typeVariable removed typevariable
	 * @param replacee replacing type variable for right side occurences of typevariable
	 * @return new Substituion instances
	 * @throws AppendableException 
	 */
	public Substitution removeTypeVariable(TypeVariable typeVariable, TypeVariable replacee) throws AppendableException {
		Set<Pair<TypeVariable, Type>> s = this.stream().collect(Collectors.toSet());
		List<Pair<TypeVariable, Type>> r = new LinkedList<Pair<TypeVariable, Type>>();
		
		for(Pair<TypeVariable, Type> p : s) {
			Type t = p.second.replaceVariable(typeVariable, replacee);
			if(!(p.first.equals(typeVariable))) {
				r.add(new Pair<TypeVariable, Type>(p.first, t));
			}
			else{
				r.add(new Pair<TypeVariable, Type>(replacee, t));
			}
		}
		
		return new Substitution(r);
	}
	
	/**
	 * Removes typeVariable from all lefts sides of subsitution and replaces it with new unique variable on right sides
	 * 
	 * @param typeVariable removed type variable
	 * @return new Substitution instance
	 * @throws AppendableException
	 */
	public Substitution removeTypeVariable(TypeVariable typeVariable) throws AppendableException {
		return this.removeTypeVariable(typeVariable, new TypeVariable(NameGenerator.next()));
	}
	
	/**
	 * Removes all type variables in key set of replace map from left sides of the substitution and replaces them on right side by their mapped values
	 * 
	 * @param replaceMap map specifing what variables will be removed and by what type variable will they be replaced
	 * @return new Substitution instane
	 * @throws AppendableException
	 */
	public Substitution removeTypeVariables(Map<TypeVariable, TypeVariable> replaceMap) throws AppendableException {
		Substitution s = this;
		for(Map.Entry<TypeVariable, TypeVariable> e : replaceMap.entrySet()) {
			s = s.removeTypeVariable(e.getKey(), e.getValue());
		}
		return s;
	}
	
	/**
	 * Removes all type variables in key set of replace map from left sides of the substitution and replaces them on right side by new unique type variables
	 * 
	 * @param typeVariables removed type variables
	 * @return new Substitution instance
	 * @throws AppendableException
	 */
	public Substitution removeTypeVariables(Set<TypeVariable> typeVariables) throws AppendableException{
		Substitution s = this;
		for(TypeVariable tv : typeVariables) {
			s = s.removeTypeVariable(tv, new TypeVariable(NameGenerator.next()));
		}
		return s;		
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof Substitution) {
			return this.elements.equals(((Substitution) other).elements);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.elements.hashCode();
	}
	
	/**
	 * Substitution collector
	 */
	public static final Collector<Pair<TypeVariable, Type>, ArrayList<Pair<TypeVariable, Type>>, Substitution> toSubstitution = 
	Collector.of(
			ArrayList<Pair<TypeVariable, Type>>::new,
			ArrayList<Pair<TypeVariable, Type>>::add,
			(ArrayList<Pair<TypeVariable, Type>> left, ArrayList<Pair<TypeVariable, Type>> right) -> { left.addAll(right); return left; },
			(l) -> new Substitution(l));

	/**
	 * Empty substitution
	 */
	public static final Substitution EMPTY = new Substitution();
}
