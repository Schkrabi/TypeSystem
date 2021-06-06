package velka.lang.types;

import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.stream.Stream;

import velka.lang.util.AppendableException;
import velka.lang.util.Pair;

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
	 * Composes this substitution with another to new substitution
	 * @param other substitution
	 * @return Optinal.Empty if substitutions cannot be merged. Otherwise Optional with merged substitution
	 */
	public Optional<Substitution> union(Substitution other){
		Set<TypeVariable> intersection = new TreeSet<TypeVariable>();
		intersection.addAll(this.elements.keySet());
		intersection.retainAll(other.elements.keySet());
		
		Substitution unifier = Substitution.EMPTY;

		for (TypeVariable v : intersection) {
			Optional<Substitution> mgu = Type.unifyTypes(this.get(v).get(), other.get(v).get());
			if(mgu.isEmpty()) {
				return Optional.empty();
			}
			
			Optional<Substitution> optUnifier = unifier.union(mgu.get()); 
			if(optUnifier.isEmpty()) {
				return Optional.empty();
			}
			
			unifier = optUnifier.get();
		}
		
		List<Pair<TypeVariable, Type>> l = new LinkedList<Pair<TypeVariable, Type>>();
		
		for(Map.Entry<TypeVariable, Type> e : this.elements.entrySet()){
			TypeVariable var = e.getKey();
			Type t = e.getValue().apply(unifier);
			Pair<TypeVariable, Type> p = new Pair<TypeVariable, Type>(var, t);
			l.add(p);
		}
		
		for(Map.Entry<TypeVariable, Type> e : other.elements.entrySet()){
			TypeVariable var = e.getKey();
			Type t = e.getValue().apply(unifier);
			Pair<TypeVariable, Type> p = new Pair<TypeVariable, Type>(var, t);
			l.add(p);
		}
		
		Substitution composition = new Substitution(l);

		return Optional.of(composition);
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

	public static Optional<Substitution> unionMany(Collection<Substitution> substitutions) throws AppendableException {
		Substitution agg = Substitution.EMPTY;
		
		for(Substitution s : substitutions) {
			Optional<Substitution> opt = agg.union(s);
			if(opt.isEmpty()) {
				return Optional.empty();
			}
			agg = opt.get();
		}
		
		return Optional.of(agg);
	}

	/**
	 * Empty substitution
	 */
	public static final Substitution EMPTY = new Substitution();
}
