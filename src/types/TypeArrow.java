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
		if(toType instanceof TypeVariable){
			return expr;
		}
		if(!(toType instanceof TypeArrow)){
			this.throwConversionError(expr, toType);
		}
		TypeArrow t = (TypeArrow)toType;
		Variable v = new Variable(NameGenerator.next());
		Lambda l = new Lambda(v, this.ltype.convertTo(expr, t.ltype));
		l.infer();
		Expression e = this.rtype.convertTo(l, t.rtype);
		return e;
	}

	@Override
	public Expression convertToDefaultRepresentation(Expression expr) throws Exception {
		Variable v = new Variable(NameGenerator.next());
		Lambda l = new Lambda(v, this.ltype.convertToDefaultRepresentation(expr));
		l.infer();
		return this.rtype.convertToDefaultRepresentation(l);
	}
	
	/**
	 * Unfolds the applicable type to TypeArrow
	 * @param t unfolded type
	 * @return TypeArrow type
	 * @throws Exception if unfolded type is not an applicable type
	 */
	public static TypeArrow getFunctionType(Type t) throws Exception{
		Type type = t.getRep();
		if(type instanceof TypeArrow){
			return (TypeArrow)type;
		}
		else if(type instanceof ForallType){
			return getFunctionType(((ForallType)type).getBoundType());
		}
		else{
			throw new Exception(t.toString() + " is not an applicable type");
		}
	}
}
