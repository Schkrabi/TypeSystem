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
import expression.Symbol;
import expression.Tuple;
import interpretation.Environment;
import util.AppendableException;
import util.NameGenerator;
import util.Pair;

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
		if (toType instanceof TypeVariable || this.equals(toType)) {
			return expr;
		}
		if (!(toType instanceof TypeTuple) || (this.size() != ((TypeTuple) toType).size())) {
			throw new ConversionException(this, toType, expr);
		}

		final Symbol symbol = new Symbol(NameGenerator.next());
		List<Expression> l = new LinkedList<Expression>();
		TypeTuple toTuple = (TypeTuple) toType;
		Iterator<Type> i = this.iterator();
		Iterator<Type> j = toTuple.iterator();
		int k = 0;
		boolean anyConverted = false;
		while (i.hasNext()) {
			Type from = i.next();
			Type to = j.next();
			final int constK = k;

			Expression e = new Expression() {

				@Override
				public Expression interpret(Environment env) throws AppendableException {
					Expression e = symbol.interpret(env);
					if(!(e instanceof Tuple)) {
						throw new AppendableException("Expected tuple got " + e.toString());
					}
					Tuple t = (Tuple) e;
					// Should not interpret converted value again as it will already be bound
					// interpreted
					return t.get(constK);
				}

				@Override
				public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
					return new Pair<Type, Substitution>(from, Substitution.EMPTY);
				}

				@Override
				public String toClojureCode(Environment env) throws AppendableException {
					return "(get " + symbol.toClojureCode(env) + " " + constK + ")";
				}
				
				@Override
				public String toString() {
					return "(get " + symbol.toString() + " " + constK + ")";
				}

			};
			
			Expression converted = from.convertTo(e, to);
			if(!anyConverted && !converted.equals(e)) {
				anyConverted = true;
			}

			l.add(converted);
			k++;
		}
		
		if(!anyConverted) {
			return expr;
		}

		Tuple conversionTuple = new Tuple(l) {
			@Override
			public Expression interpret(Environment env) throws AppendableException {
				Expression e = expr.interpret(env);
				Environment bound = Environment.create(env);
				bound.put(symbol, e);
				return super.interpret(bound);
			}

			@Override
			public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
				Pair<Type, Substitution> p = expr.infer(env);
				return new Pair<Type, Substitution>(toType, p.second);
			}

			@Override
			public String toClojureCode(Environment env) throws AppendableException {
				return "((fn [" + symbol.toClojureCode(env) + "] " + super.toClojureCode(env) + ") "
						+ expr.toClojureCode(env) + ")";
			}
		};

		return conversionTuple;
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

			if (!ti.equalsUpToIsomorphism(tj))
				sum++;
		}

		return sum;
	}

	@Override
	public Substitution unifyWith(Type other) throws AppendableException {
		if (other instanceof TypeVariable || other instanceof RepresentationOr) {
			return other.unifyWith(this);
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

					Substitution ot = Type.unify(t, u);

					s = s.union(ot);
				}
				return s;
			}
		}
		throw new TypesDoesNotUnifyException(this, other);
	}

	@Override
	public String clojureTypeRepresentation() throws AppendableException {
		StringBuilder s = new StringBuilder("[");
		Iterator<Type> i = this.values.iterator();
		while (i.hasNext()) {
			Type t = i.next();
			s.append(t.clojureTypeRepresentation());
			if (i.hasNext()) {
				s.append(" ");
			}
		}
		s.append("]");
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
}
