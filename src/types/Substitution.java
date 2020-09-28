package types;

import java.util.Collection;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.stream.Stream;

import util.AppendableException;
import util.Pair;

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
		super();
	}

	public Substitution(Collection<Pair<TypeVariable, Type>> init) {
		super();
		init.stream().forEach(x -> this.elements.put(x.first, x.second));
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
	 * 
	 * @param other substitution
	 * @return new substitution object
	 * @throws AppendableException
	 */
	public Substitution union(Substitution other) throws AppendableException {
		Set<TypeVariable> intersection = new TreeSet<TypeVariable>();
		intersection.addAll(this.elements.keySet());
		intersection.retainAll(other.elements.keySet());

		Substitution composition = new Substitution();
		composition.elements.putAll(this.elements);
		composition.elements.putAll(other.elements);
		for (TypeVariable v : intersection) {
			composition.elements.remove(v);
		}

		for (TypeVariable v : intersection) {
			Substitution mgu = Type.unifyTypes(this.get(v).get(), other.get(v).get());
			composition.elements.put(v, this.get(v).get().apply(mgu));
			composition = composition.union(mgu);
		}

		return composition;
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

	/**
	 * Empty substitution
	 */
	public static final Substitution EMPTY = new Substitution();
}
