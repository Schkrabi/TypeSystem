package types;

import java.util.Set;
import java.util.TreeSet;

import expression.Expression;

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
		/*if (this.getRep() == this) {
			s.add(this);
			return s;
		}
		s.addAll(this.getRep().getUnconstrainedVariables());*/
		s.add(this);
		return s;
	}

	@Override
	public Expression convertTo(Expression expr, Type toType) {
		return expr;
	}

	@Override
	public boolean isAtomicType() {
		return true;
	}

	@Override
	public Type apply(Substitution s) {
		if(s.containsKey(this)) {
			Type t = s.get(this), v;
			do {
				v = t.apply(s);
			}while(!v.equals(t));
			
			return t;			
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
