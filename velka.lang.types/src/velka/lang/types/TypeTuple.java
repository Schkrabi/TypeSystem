package velka.lang.types;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.Vector;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import velka.lang.util.AppendableException;

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
	public Type apply(Substitution s) {
		return new TypeTuple(this.stream().map(x -> x.apply(s)).collect(Collectors.toList()));
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

			try {
				Type.unifyRepresentation(ti, tj);
			}catch (AppendableException e) {
				sum++;
			}
		}

		return sum;
	}

	@Override
	public Substitution unifyTypeWith(Type other) throws AppendableException {
		if (other instanceof TypeVariable || other instanceof RepresentationOr) {
			return other.unifyTypeWith(this);
		}
		if (other instanceof TypeTuple) {
			TypeTuple o = (TypeTuple) other;
			if (this.size() == o.size()) {
				Substitution s = Substitution.EMPTY;

				Iterator<Type> i = this.iterator();
				Iterator<Type> j = o.iterator();
				while (i.hasNext()) {
					Type t = i.next();
					Type u = j.next();

					Substitution ot = Type.unifyTypes(t, u);

					s = s.union(ot);
				}
				return s;
			}
		}
		throw new TypesDoesNotUnifyException(this, other);
	}
	
	@Override
	public Substitution unifyRepresentationWith(Type other) throws AppendableException {
		if (other instanceof TypeVariable || other instanceof RepresentationOr) {
			return other.unifyRepresentationWith(this);
		}
		if (other instanceof TypeTuple) {
			TypeTuple o = (TypeTuple) other;
			if (this.size() == o.size()) {
				Substitution s = Substitution.EMPTY;

				Iterator<Type> i = this.iterator();
				Iterator<Type> j = o.iterator();
				while (i.hasNext()) {
					Type t = i.next();
					Type u = j.next();

					Substitution ot = Type.unifyRepresentation(t, u);

					s = s.union(ot);
				}
				return s;
			}
		}
		throw new TypesDoesNotUnifyException(this, other);
	}

	@Override
	public String clojureTypeRepresentation() throws AppendableException {
		StringBuilder s = new StringBuilder("(new velka.lang.types.TypeTuple [");
		Iterator<Type> i = this.values.iterator();
		while (i.hasNext()) {
			Type t = i.next();
			s.append(t.clojureTypeRepresentation());
			if (i.hasNext()) {
				s.append(" ");
			}
		}
		s.append("])");
		return s.toString();
	}

	@Override
	public Type uniteRepresentationsWith(Type other) throws AppendableException {
		if (other instanceof RepresentationOr || other instanceof TypeVariable) {
			return other.uniteRepresentationsWith(this);
		}
		if (!(other instanceof TypeTuple) || ((TypeTuple) other).size() != this.size()) {
			throw new AppendableException("Cannot unite types " + this + " " + other);
		}

		Iterator<Type> i = this.iterator();
		Iterator<Type> j = ((TypeTuple) other).iterator();
		List<Type> l = new LinkedList<Type>();
		while (i.hasNext()) {
			Type t = i.next();
			Type o = j.next();
			l.add(t.uniteRepresentationsWith(o));
		}

		return new TypeTuple(l);
	}

	@Override
	public Type map(Function<Type, Type> fun) throws AppendableException {
		Type t = null;
		try {
			t = new TypeTuple(this.stream().map(velka.lang.util.ThrowingFunction.wrapper(x -> x.map(fun))).collect(Collectors.toList()));
		}catch(RuntimeException re) {
			AppendableException e = (AppendableException) re.getCause();
			throw e;
		}
		return t;
	}
}
