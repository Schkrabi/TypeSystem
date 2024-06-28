package velka.types;

import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.BiFunction;
import java.util.function.BinaryOperator;
import java.util.function.Function;

import velka.types.TypeArrow;
import velka.util.AppendableException;
import velka.util.ClojureHelper;

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
		if(this == o) return true;
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
			if(left.isEmpty()) {
				return Optional.empty();
			}
			
			Optional<Substitution> right = this.rtype.apply(left.get()).unifyTypeWith(o.rtype.apply(left.get()));
			
			if(right.isEmpty()) {
				return Optional.empty();
			}

			return Optional.of(left.get().compose(right.get()));
		}
		return Optional.empty();
	}
	
	@Override
	public Optional<Substitution> unifyRepresentationWith(Type other) {
		if (other instanceof TypeVariable || other instanceof RepresentationOr) {
			return other.unifyTypeWith(this);
		}
		if (other instanceof TypeArrow) {
			TypeArrow o = (TypeArrow) other;

			Optional<Substitution> left = this.ltype.unifyRepresentationWith(o.ltype);
			if(left.isEmpty()) {
				return Optional.empty();
			}
			
			Optional<Substitution> right = this.rtype.apply(left.get()).unifyRepresentationWith(o.rtype.apply(left.get()));
			
			if(right.isEmpty()) {
				return Optional.empty();
			}

			return Optional.of(left.get().compose(right.get()));
		}
		return Optional.empty();
	}

	@Override
	public String clojureTypeRepresentation() throws AppendableException {
		String code = ClojureHelper.instantiateJavaClass(
						this.getClass(),
						this.ltype.clojureTypeRepresentation(),
						this.rtype.clojureTypeRepresentation());
		
		return code;
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
	public Type replaceVariable(TypeVariable replaced, TypeVariable replacee) throws AppendableException {
		return new TypeArrow(this.ltype.replaceVariable(replaced, replacee), this.rtype.replaceVariable(replaced, replacee));
	}

	@Override
	public <R> R reduce(Function<TerminalType, R> mapFun, BinaryOperator<R> combiner, R terminator)
			throws AppendableException {
		R agg = terminator;
		R lresult = this.ltype.reduce(mapFun, combiner, terminator);
		agg = combiner.apply(agg, lresult);
		
		R rresult = this.rtype.reduce(mapFun, combiner, terminator);
		agg = combiner.apply(agg, rresult);		
		
		return agg;
	}

	@Override
	protected <R> R doMap2AndReduce(Type other, BiFunction<Type, Type, R> mapFun,
			BinaryOperator<R> combinator, R terminator) throws AppendableException {
		if(other instanceof TypeArrow) {
			throw new AppendableException("Cannot map2AndReduce " + this.toString() + " with " + other.toString());
		}
		TypeArrow o = (TypeArrow)other;
		
		R agg = terminator;
		R lresult = this.ltype.map2AndReduce(o.ltype, mapFun, combinator, terminator);
		agg = combinator.apply(agg, lresult);
		
		R rresult = this.rtype.map2AndReduce(o.rtype, mapFun, combinator, terminator);
		agg = combinator.apply(agg, rresult);
		
		return agg;
	}

	@Override
	public boolean doCanConvertTo(Type other, BiFunction<TypeAtom, TypeAtom, Boolean> atomCheck) {
		if(!(other instanceof TypeArrow)) {
			return false;
		}
		TypeArrow o = (TypeArrow)other;
		
		Boolean canConvertL = this.ltype.canConvertTo(o.ltype, atomCheck);
		Boolean canConvertR = this.rtype.canConvertTo(o.rtype, atomCheck);
		
		return canConvertL && canConvertR;
	}
}
