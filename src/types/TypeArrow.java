package types;

import java.util.Arrays;
import java.util.Set;
import java.util.TreeSet;

import abstraction.Lambda;
import application.Application;
import util.AppendableException;
import util.NameGenerator;
import expression.Expression;
import expression.Tuple;
import expression.Symbol;

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
		Symbol v = new Symbol(NameGenerator.next());

		Lambda l = new Lambda(new Tuple(Arrays.asList(v)), (TypeTuple) t.ltype, this.rtype.convertTo(
				new Application(expr, (Tuple) t.ltype.convertTo(new Tuple(Arrays.asList(v)), this.ltype)), t.rtype));
		return l;
	}

	@Override
	public String convertToClojure(String argument, Type toType) throws AppendableException {
		if (toType instanceof TypeVariable) {
			return argument;
		}
		if (!(toType instanceof TypeArrow)) {
			throw new ClojureConversionException(this, toType, argument);
		}
		TypeArrow t = (TypeArrow) toType;
		String v = NameGenerator.next();

		return "(fn [" + v + "] " + this.rtype
				.convertToClojure("(" + argument + " " + t.ltype.convertToClojure(v, this.ltype) + ")", t.rtype) + ")";
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

	@Override
	public Substitution unifyWith(Type other) throws AppendableException {
		if (other instanceof TypeVariable || other instanceof RepresentationOr) {
			return other.unifyWith(this);
		}
		if (other instanceof TypeArrow) {
			TypeArrow o = (TypeArrow) other;

			Substitution left = this.ltype.unifyWith(o.ltype);
			Substitution right = this.rtype.unifyWith(o.rtype);

			return left.union(right);
		}
		throw new TypesDoesNotUnifyException(this, other);
	}

	@Override
	public String toClojure() throws AppendableException {
		throw new AppendableException("toClojure of " + this.getClass().getName() + " : " + this + " is not allowed");
	}

	/**
	 * Returns true if this type is representing expression that can be applicated
	 * (function, lambda, extended function or lambda). Otherwise returns false.
	 * 
	 * @return true or false.
	 */
	public boolean isApplicableType() {
		return true;
	}

	@Override
	public Type uniteRepresentationsWith(Type other) throws AppendableException {
		if (other instanceof RepresentationOr || other instanceof TypeVariable) {
			return other.uniteRepresentationsWith(this);
		}
		if (!(other instanceof TypeArrow)) {
			throw new AppendableException("Cannot unite types " + this + " " + other);
		}
		TypeArrow o = (TypeArrow) other;

		return new TypeArrow(this.ltype.uniteRepresentationsWith(o.ltype),
				this.rtype.uniteRepresentationsWith(o.rtype));
	}
}
