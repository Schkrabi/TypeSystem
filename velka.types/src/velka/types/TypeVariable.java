package velka.types;

import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.function.Function;

import velka.types.TypeVariable;
import velka.util.AppendableException;
import velka.util.NameGenerator;
import velka.util.Pair;

/**
 * Type variable
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class TypeVariable extends Type {
	/**
	 * Name of the variable
	 */
	public final String name;

	public TypeVariable(String name) {
		this.name = name;
	}

	@Override
	public String toString() {
		return this.name;
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof TypeVariable)) {
			return false;
		}
		TypeVariable other = (TypeVariable) o;
		return this.name.equals(other.name);
	}

	@Override
	public int compareTo(Type o) {
		if (!(o instanceof TypeVariable)) {
			return super.compareTo(o);
		}
		TypeVariable arg = (TypeVariable) o;
		return this.name.compareTo(arg.name);
	}

	@Override
	public Set<TypeVariable> getVariables() {
		Set<TypeVariable> s = new TreeSet<TypeVariable>();
		s.add(this);
		return s;
	}

	@Override
	public Type apply(Substitution s) {
		Type t = this;
		Set<TypeVariable> history = new TreeSet<TypeVariable>();
		while(t instanceof TypeVariable) {
			TypeVariable tv = (TypeVariable)t;
			if(!s.containsVariable(tv)) {
				return tv;
			}
			history.add(tv);
			t = s.get(tv).get();
			if(history.contains(t))
				return t;
		}
			
		return t.apply(s);
	}

	@Override
	public int hashCode() {
		return this.name.hashCode();
	}

	@Override
	public Optional<Substitution> unifyTypeWith(Type other) {
		if (this.equals(other)) {
			return Optional.of(Substitution.EMPTY);
		}
		return Optional.of(new Substitution(Arrays.asList(new Pair<TypeVariable, Type>(this, other))));
	}
	
	@Override
	public Optional<Substitution> unifyRepresentationWith(Type other) {
		return this.unifyTypeWith(other);
	}

	@Override
	public String clojureTypeRepresentation() throws AppendableException {
		return "(new velka.types.TypeVariable \"" + this.name + "\")";
	}

	@Override
	public Type uniteRepresentationsWith(Type other){
		return other;
	}

	@Override
	public Type map(Function<Type, Type> fun) throws AppendableException {
		return fun.apply(this);
	}

	@Override
	public Type replaceVariable(TypeVariable replaced, TypeVariable replacee) {
		if(this.equals(replaced)) {
			return replacee;
		}
		return this;
	}
	
	/**
	 * Creates a map that has a new unique typevariable mapped to every type
	 * variable in variables
	 * 
	 * @param variables mapped variables
	 * @return new TreeMap instance
	 */
	public static Map<TypeVariable, TypeVariable> makeRenameMap(Collection<TypeVariable> variables) {
		Map<TypeVariable, TypeVariable> m = new TreeMap<TypeVariable, TypeVariable>();
		for (TypeVariable tv : variables) {
			m.put(tv, new TypeVariable(NameGenerator.next()));
		}
		return m;
	}
}
