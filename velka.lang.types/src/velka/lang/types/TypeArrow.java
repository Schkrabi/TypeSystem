package velka.lang.types;

import java.util.Set;
import java.util.TreeSet;
import java.util.function.Function;

import velka.lang.util.AppendableException;

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
	public Type apply(Substitution s) {
		return new TypeArrow(this.ltype.apply(s), this.rtype.apply(s));
	}

	@Override
	public int hashCode() {
		return this.ltype.hashCode() * this.rtype.hashCode();
	}

	@Override
	public Substitution unifyTypeWith(Type other) throws AppendableException {
		if (other instanceof TypeVariable || other instanceof RepresentationOr) {
			return other.unifyTypeWith(this);
		}
		if (other instanceof TypeArrow) {
			TypeArrow o = (TypeArrow) other;

			Substitution left = this.ltype.unifyTypeWith(o.ltype);
			Substitution right = this.rtype.unifyTypeWith(o.rtype);

			return left.union(right);
		}
		throw new TypesDoesNotUnifyException(this, other);
	}
	
	@Override
	public Substitution unifyRepresentationWith(Type other) throws AppendableException {
		if (other instanceof TypeVariable || other instanceof RepresentationOr) {
			return other.unifyRepresentationWith(this);
		}
		if (other instanceof TypeArrow) {
			TypeArrow o = (TypeArrow) other;

			Substitution left = this.ltype.unifyRepresentationWith(o.ltype);
			Substitution right = this.rtype.unifyRepresentationWith(o.rtype);

			return left.union(right);
		}
		throw new TypesDoesNotUnifyException(this, other);
	}

	@Override
	public String clojureTypeRepresentation() throws AppendableException {
		return "(lang-type-arrow. " + this.ltype.clojureTypeRepresentation() + " "
				+ this.rtype.clojureTypeRepresentation() + ")";
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

	@Override
	public Type map(Function<Type, Type> fun) throws AppendableException {
		return new TypeArrow(this.ltype.map(fun), this.rtype.map(fun));
	}
}
