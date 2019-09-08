package types;

import java.util.Arrays;
import java.util.Set;
import java.util.TreeSet;

import util.AppendableException;
import util.NameGenerator;
import expression.Application;
import expression.Expression;
import expression.Lambda;
import expression.Tuple;
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
		s.addAll(this.ltype.getUnconstrainedVariables());
		s.addAll(this.rtype.getUnconstrainedVariables());
		return s;
	}

	@Override
	public int compareTo(Type o) {
		if (!(o instanceof TypeArrow)) {
			return super.compareTo(o);
		}
		TypeArrow other = (TypeArrow) o;
		int cmp = this.ltype.compareTo(other.ltype);
		if (cmp != 0)
			return cmp;
		return this.rtype.compareTo(other.rtype);
	}

	@Override
	public Expression convertTo(Expression expr, Type toType) throws AppendableException {
		if (toType instanceof TypeVariable) {
			return expr;
		}
		if (!(toType instanceof TypeArrow)) {
			throw new ConversionException(this, toType, expr);
		}
		TypeArrow t = (TypeArrow) toType;
		Variable v = new Variable(NameGenerator.next());

		Lambda l = new Lambda(new Tuple(Arrays.asList(v)), (TypeTuple) t.ltype, this.rtype.convertTo(
				new Application(expr, (Tuple) t.ltype.convertTo(new Tuple(Arrays.asList(v)), this.ltype)), t.rtype));
		return l;
	}

	@Override
	public Type apply(Substitution s) {
		return new TypeArrow(this.ltype.apply(s), this.rtype.apply(s));
	}

	@Override
	public Type removeRepresentationInfo() {
		return new TypeArrow(this.ltype.removeRepresentationInfo(), this.rtype.removeRepresentationInfo());
	}

	@Override
	public int hashCode() {
		return this.ltype.hashCode() * this.rtype.hashCode();
	}
}
