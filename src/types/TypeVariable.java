package types;

import java.util.Set;
import java.util.TreeSet;

import expression.Expression;
import interpretation.Environment;

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
	public Expression convertTo(Expression expr, Type toType) {
		return expr;
	}

	@Override
	public Type apply(Substitution s) {
		if(s.containsVariable(this)) {
			Type t = s.get(this).get();			
			return t.apply(s);			
		}
		return this;
	}

	@Override
	public Type removeRepresentationInfo() {
		return this;
	}
	
	@Override
	public int hashCode() {
		return this.name.hashCode();
	}
}
