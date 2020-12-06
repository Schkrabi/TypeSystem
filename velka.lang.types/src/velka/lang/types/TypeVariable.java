package velka.lang.types;

import java.util.Arrays;
import java.util.Set;
import java.util.TreeSet;

import velka.lang.util.AppendableException;
import velka.lang.util.Pair;

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
	public Set<TypeVariable> getUnconstrainedVariables() {
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
	public Substitution unifyTypeWith(Type other) {
		if (this.equals(other)) {
			return Substitution.EMPTY;
		}
		return new Substitution(Arrays.asList(new Pair<TypeVariable, Type>(this, other)));
	}
	
	@Override
	public Substitution unifyRepresentationWith(Type other) throws AppendableException {
		return this.unifyTypeWith(other);
	}

	@Override
	public String clojureTypeRepresentation() throws AppendableException {
		return "\"" + this.name + "\"";
	}

	@Override
	public Type uniteRepresentationsWith(Type other){
		return other;
	}
}
