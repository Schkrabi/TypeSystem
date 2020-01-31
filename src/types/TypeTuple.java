package types;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.Vector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import expression.Expression;
import expression.Tuple;
import util.AppendableException;
import util.NameGenerator;

/**
 * Tuple of types
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public class TypeTuple extends Type implements Iterable<Type> {

	/**
	 * Values of the tuple
	 */
	private final Vector<Type> values;

	/**
	 * Empty type tuple object
	 */
	public static final TypeTuple EMPTY_TUPLE = new TypeTuple();

	public TypeTuple(Collection<? extends Type> values) {
		this.values = new Vector<Type>(values);
	}

	private TypeTuple() {
		this.values = new Vector<Type>();
	}

	/**
	 * Gets expression on tuple index
	 * 
	 * @param index searched index
	 * @return element on given index
	 */
	public Type get(int index) {
		return this.values.get(index);
	}

	/**
	 * Gets size of this tuple
	 * 
	 * @return integer
	 */
	public int size() {
		return this.values.size();
	}

	/**
	 * Gets stream of this tuple
	 * 
	 * @return
	 */
	public Stream<Type> stream() {
		return this.values.stream();
	}

	@Override
	public String toString() {
		StringBuilder s = new StringBuilder("[");
		Iterator<Type> i = this.iterator();
		while (i.hasNext()) {
			Type t = i.next();
			s.append(t.toString());
			if (i.hasNext()) {
				s.append(", ");
			}
		}
		s.append("]");
		return s.toString();
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof TypeTuple)) {
			return false;
		}
		TypeTuple other = (TypeTuple) o;
		return this.values.equals(other.values);
	}

	@Override
	public Set<TypeVariable> getUnconstrainedVariables() {
		Set<TypeVariable> s = new TreeSet<TypeVariable>();

		for (Type t : this) {
			s.addAll(t.getUnconstrainedVariables());
		}
		return s;
	}

	@Override
	public int compareTo(Type o) {
		if (!(o instanceof TypeTuple)) {
			return super.compareTo(o);
		}
		TypeTuple other = (TypeTuple) o;

		if (this.values.size() != other.values.size()) {
			return Integer.compare(this.values.size(), other.values.size());
		}

		Iterator<Type> i = this.iterator();
		Iterator<Type> j = other.iterator();

		while (i.hasNext()) {
			Type t = i.next();
			Type u = j.next();

			int cmp = t.compareTo(u);
			if (cmp != 0)
				return cmp;
		}
		return 0;
	}

	@Override
	public Iterator<Type> iterator() {
		return this.values.iterator();
	}

	@Override
	public Expression convertTo(Expression expr, Type toType) throws AppendableException {
		if (toType instanceof TypeVariable) {
			return expr;
		}
		if (!(toType instanceof TypeTuple) || (!(expr instanceof Tuple)) || (this.size() != ((TypeTuple) toType).size())
				|| (this.size() != ((Tuple) expr).size())) {
			throw new ConversionException(this, toType, expr);
		}
		TypeTuple ttpl = (TypeTuple) toType;
		Tuple tpl = (Tuple) expr;

		List<Expression> ts = new LinkedList<Expression>();
		Iterator<Expression> i = tpl.iterator();
		Iterator<Type> j = this.iterator();
		Iterator<Type> k = ttpl.iterator();

		while (i.hasNext()) {
			Expression e = i.next();
			Type t = j.next();
			Type u = k.next();

			ts.add(t.convertTo(e, u));
		}

		return new Tuple(ts);
	}

	@Override
	public String convertToClojure(String argument, Type toType) throws AppendableException {
		if (toType instanceof TypeVariable) {
			return argument;
		}
		if (!(toType instanceof TypeTuple) || (this.size() != ((TypeTuple) toType).size())) {
			throw new ClojureConversionException(this, toType, argument);
		}
		TypeTuple ttpl = (TypeTuple) toType;
		String v = NameGenerator.next();

		StringBuilder s = new StringBuilder("((fn [");
		s.append(v);
		s.append("] [");

		Iterator<Type> i = this.iterator();
		Iterator<Type> j = ttpl.iterator();
		int n = 0;

		while (i.hasNext()) {
			Type from = i.next();
			Type to = j.next();
			String arg = "(get " + v  + " " + Integer.toString(n) + ")";

			s.append(from.convertToClojure(arg, to));
			if (i.hasNext()) {
				s.append(" ");
			}

			n++;
		}
		s.append("]) ");
		s.append(argument);
		s.append(")");
		return s.toString();
	}

	@Override
	public Type apply(Substitution s) {
		return new TypeTuple(this.stream().map(x -> x.apply(s)).collect(Collectors.toList()));
	}

	@Override
	public Type removeRepresentationInfo() {
		return new TypeTuple(this.stream().map(x -> x.removeRepresentationInfo()).collect(Collectors.toList()));
	}

	@Override
	public int hashCode() {
		return this.values.hashCode();
	}

	/**
	 * Returns somewhat distance of two typetuples
	 * 
	 * @param other other type tuple
	 * @return integer
	 */
	public int tupleDistance(TypeTuple other) {
		if (this.size() != other.size())
			return Math.abs(this.size() - other.size());

		Iterator<Type> i = this.iterator();
		Iterator<Type> j = other.iterator();
		int sum = 0;
		while (i.hasNext()) {
			Type ti = i.next();
			Type tj = j.next();

			if (!ti.equals(tj))
				sum++;
		}

		return sum;
	}

	@Override
	public Substitution unifyWith(Type other) throws AppendableException {
		if(other instanceof TypeVariable
				|| other instanceof RepresentationOr) {
			return other.unifyWith(this);
		}
		if(other instanceof TypeTuple) {
			TypeTuple o = (TypeTuple)other;
			if(this.size() == o.size()) {
				Substitution s = Substitution.EMPTY;

				Iterator<Type> i = this.iterator();
				Iterator<Type> j = o.iterator();
				while (i.hasNext()) {
					Type t = i.next();
					Type u = j.next();

					Substitution ot = Type.unify(t, u);

					s = s.union(ot);
				}
				return s;
			}
		}
		throw new TypesDoesNotUnifyException(this, other);
	}

	@Override
	public String toClojure() throws AppendableException {
		StringBuilder s = new StringBuilder("[");
		Iterator<Type> i = this.values.iterator();
		while(i.hasNext()) {
			Type t = i.next();
			s.append(t.toClojure());
			if(i.hasNext()) {
				s.append(" ");
			}
		}
		s.append("]");
		return s.toString();
	}
}
