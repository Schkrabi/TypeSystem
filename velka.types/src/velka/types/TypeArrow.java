package velka.types;

import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Function;

import velka.types.RepresentationOr;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeVariable;
import velka.util.AppendableException;

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
	public Set<TypeVariable> getVariables() {
		Set<TypeVariable> s = new TreeSet<TypeVariable>();
		s.addAll(this.ltype.getVariables());
		s.addAll(this.rtype.getVariables());
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
	public Optional<Substitution> unifyTypeWith(Type other) {
		if (other instanceof TypeVariable || other instanceof RepresentationOr) {
			return other.unifyTypeWith(this);
		}
		if (other instanceof TypeArrow) {
			TypeArrow o = (TypeArrow) other;

			Optional<Substitution> left = this.ltype.unifyTypeWith(o.ltype);
			Optional<Substitution> right = this.rtype.unifyTypeWith(o.rtype);
			
			if(left.isEmpty() || right.isEmpty()) {
				return Optional.empty();
			}

			return left.get().union(right.get());
		}
		return Optional.empty();
	}
	
	@Override
	public Optional<Substitution> unifyRepresentationWith(Type other) {
		if (other instanceof TypeVariable || other instanceof RepresentationOr) {
			return other.unifyRepresentationWith(this);
		}
		if (other instanceof TypeArrow) {
			TypeArrow o = (TypeArrow) other;

			Optional<Substitution> left = this.ltype.unifyRepresentationWith(o.ltype);
			Optional<Substitution> right = this.rtype.unifyRepresentationWith(o.rtype);
			
			if(left.isEmpty() || right.isEmpty()) {
				return Optional.empty();
			}

			return left.get().union(right.get());
		}
		return Optional.empty();
	}

	@Override
	public String clojureTypeRepresentation() throws AppendableException {
		return "(new velka.lang.types.TypeArrow " + this.ltype.clojureTypeRepresentation() + " "
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

	@Override
	protected Type replaceVariable(TypeVariable replaced, TypeVariable replacee) throws AppendableException {
		return new TypeArrow(this.ltype.replaceVariable(replaced, replacee), this.rtype.replaceVariable(replaced, replacee));
	}
}
