package types;

import java.util.Set;
import java.util.TreeSet;

import util.NameGenerator;

import expression.Expression;
import expression.Lambda;
import expression.Variable;

/**
 * Class for functions types
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class TypeArrow extends Type {
	/**
	 * left type of the arrow (type of argument)
	 */
	public final Type ltype;
	/**
	 * right type of the arrow (type of return value)
	 */
	public final Type rtype;

	public TypeArrow(Type ltype, Type rtype) {
		this.ltype = ltype;
		this.rtype = rtype;
	}

	@Override
	public String toString() {
		return this.ltype.toString() + " -> " + this.rtype.toString();
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof TypeArrow)) {
			return false;
		}
		TypeArrow other = (TypeArrow) o;
		return this.ltype.equals(other.ltype) && this.rtype.equals(other.rtype);
	}

	@Override
	public Set<TypeVariable> getUnconstrainedVariables() {
		Set<TypeVariable> s = new TreeSet<TypeVariable>();
		if (this.getRep() != this) {
			s.addAll(this.getRep().getUnconstrainedVariables());
			return s;
		}

		s.addAll(this.ltype.getUnconstrainedVariables());
		s.addAll(this.rtype.getUnconstrainedVariables());
		return s;
	}

	@Override
	public boolean isApplicableType() {
		return true;
	}

	@Override
	public int compareTo(Type o) {
		if (!(o instanceof TypeArrow)) {
			return super.compareTo(o);
		}
		TypeArrow other = (TypeArrow) o;
		if (this.ltype != other.ltype) {
			return ltype.compareTo(other.ltype);
		}
		return this.rtype.compareTo(other.rtype);
	}

	@Override
	public Expression convertTo(Expression expr, Type toType) throws Exception {
		if(!(toType instanceof TypeArrow)){
			this.throwConversionError(expr, toType);
		}
		TypeArrow t = (TypeArrow)toType;
		Variable v = new Variable(NameGenerator.next());
		Expression e = this.rtype.convertTo(new Lambda(v, this.ltype.convertTo(expr, t.ltype)), t.rtype);
		return e;
	}
}
