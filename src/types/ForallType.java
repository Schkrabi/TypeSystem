package types;

import java.util.Set;
import java.util.TreeSet;

import expression.Expression;

/**
 * Universally quantified type
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class ForallType extends Type {
	/**
	 * Variable bound by this quantifier
	 */
	public final TypeVariable bound;
	/**
	 * Type in which the variable is bound
	 */
	public final Type type;

	public ForallType(TypeVariable bound, Type type) {
		this.bound = bound;
		this.type = type;
	}

	@Override
	public String toString() {
		return "Forall " + this.bound.toString() + " (" + this.type.toString() + ")";
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof ForallType)) {
			return false;
		}
		ForallType other = (ForallType) o;
		return this.bound.equals(other.bound) && this.type.equals(other.type);
	}

	@Override
	public Set<TypeVariable> getUnconstrainedVariables() {
		Set<TypeVariable> s = new TreeSet<TypeVariable>();
		if (this.getRep() != this) {
			s.addAll(this.getRep().getUnconstrainedVariables());
			return s;
		}

		s.addAll(this.type.getUnconstrainedVariables());
		s.remove(this.bound);
		return s;
	}

	@Override
	public boolean isApplicableType() {
		return this.type.isApplicableType();
	}

	public Type getBoundType() {
		Type t = this.type;
		while (t instanceof ForallType) {
			t = ((ForallType) t).type;
		}
		return t;
	}

	@Override
	public int compareTo(Type o) {
		if (!(o instanceof ForallType)) {
			return super.compareTo(o);
		}
		ForallType other = (ForallType) o;
		if (this.bound != other.bound) {
			return this.bound.compareTo(other.bound);
		}
		return this.type.compareTo(other.type);
	}

	@Override
	public Expression convertTo(Expression expr, Type toType) throws Exception {
		if(!(toType instanceof ForallType)){
			this.throwConversionError(expr, toType);
		}
		ForallType t = (ForallType)toType;
		Expression e = this.type.convertTo(expr, t);
		e.infer();
		return e;		
	}

	@Override
	public Expression convertToDefaultRepresentation(Expression expr) throws Exception {
		Expression e = this.type.convertToDefaultRepresentation(expr);
		e.infer();
		return e;
	}
}
