package types;

import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.TreeSet;
import expression.Expression;
import expression.Tuple;
import interpretation.Environment;

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
	public final Type[] values;

	/**
	 * Empty type tuple object
	 */
	public static final TypeTuple EMPTY_TUPLE = new TypeTuple(new Type[] {});

	public TypeTuple(List<Type> values) {
		Type[] ar = new Type[values.size()];
		ar = values.toArray(ar);
		this.values = ar;
	}

	public TypeTuple(Type[] values) {
		this.values = values;
	}

	@Override
	public String toString() {
		StringBuilder s = new StringBuilder("[");
		for (int i = 0; i < this.values.length; i++) {
			s.append(this.values[i].toString());
			if (i != this.values.length - 1) {
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
		if (this.values.length != other.values.length) {
			return false;
		}

		for (int i = 0; i < this.values.length; i++) {
			if (!this.values[i].equals(other.values[i])) {
				return false;
			}
		}
		return true;
	}

	@Override
	public Set<TypeVariable> getUnconstrainedVariables() {
		Set<TypeVariable> s = new TreeSet<TypeVariable>();
		/*if (this.getRep() != this) {
			s.addAll(this.getRep().getUnconstrainedVariables());
			return s;
		}*/

		for (int i = 0; i < this.values.length; i++) {
			s.addAll(this.values[i].getUnconstrainedVariables());
		}
		return s;
	}

	@Override
	public int compareTo(Type o) {
		if (!(o instanceof TypeTuple)) {
			return super.compareTo(o);
		}
		TypeTuple other = (TypeTuple) o;
		if (this.values.length != other.values.length) {
			return Integer.compare(this.values.length, other.values.length);
		}
		for (int i = 0; i < this.values.length; i++) {
			int cmp = this.values[i].compareTo(other.values[i]);
			if (cmp != 0) {
				return cmp;
			}
		}
		return 0;
	}

	@Override
	public Iterator<Type> iterator() {
		return new TypeTupleIterator();
	}

	private class TypeTupleIterator implements Iterator<Type> {

		private int cursor = 0;

		@Override
		public boolean hasNext() {
			return cursor < TypeTuple.this.values.length;
		}

		@Override
		public Type next() {
			if (this.hasNext()) {
				Type t = TypeTuple.this.values[this.cursor];
				cursor++;
				return t;
			}
			throw new NoSuchElementException();
		}

		@Override
		public void remove() {
			throw new UnsupportedOperationException();

		}

	}

	@Override
	public Expression convertTo(Expression expr, Type toType) throws Exception {
		if(toType instanceof TypeVariable){
			return expr;
		}
		if (!(toType instanceof TypeTuple) || (!(expr instanceof Tuple))
				|| (this.values.length != ((TypeTuple) toType).values.length)
				|| (this.values.length != ((Tuple) expr).values.length)) {
			this.throwConversionError(expr, toType);
		}
		TypeTuple ttpl = (TypeTuple) toType;
		Tuple tpl = (Tuple) expr;
		Expression[] ts = new Expression[ttpl.values.length];

		for (int i = 0; i < ts.length; i++) {
			Expression e = tpl.values[i];
			Type to = ttpl.values[i];
			Type from = this.values[i];
			ts[i] = from.convertTo(e, to);
		}
		
		Tuple r = new Tuple(ts);
		r.infer(new Environment());

		return r;
	}

	@Override
	public boolean isAtomicType() {
		return false;
	}

	@Override
	public Type apply(Substitution s) {
		Type[] vls = new Type[this.values.length];
		
		for(int i = 0; i < this.values.length; i++) {
			vls[i] = this.values[i].apply(s);
		}
		
		return new TypeTuple(vls);
	}
}
