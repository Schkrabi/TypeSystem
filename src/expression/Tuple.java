package expression;

import java.util.Iterator;
import java.util.NoSuchElementException;
import types.Type;
import types.TypeTuple;
import interpretation.Environment;

/**
 * Tuple expression
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public class Tuple extends Expression implements Iterable<Expression> {

	/**
	 * Values of the tuple
	 */
	public final Expression[] values;

	public Tuple(Expression[] values) {
		this.values = new Expression[values.length];
		for (int i = 0; i < values.length; i++) {
			this.values[i] = values[i];
		}
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		return this; // Should tuple intepret to itself? Probably yes due to
						// lazyness
	}

	@Override
	public String toString() {
		StringBuilder s = new StringBuilder("[");
		for (int i = 0; i < this.values.length; i++) {
			s.append(this.values[i].toString());
			if (i + 1 < this.values.length) {
				s.append(", ");
			}
		}
		s.append("]");
		return s.toString();
	}

	@Override
	public Type infer(Environment env) throws Exception {
		Type types[] = new Type[this.values.length];
		for (int i = 0; i < this.values.length; i++) {
			types[i] = this.values[i].infer(env);
		}

		Type t = new TypeTuple(types);
		this.setType(t);
		return t;
	}

	@Override
	public Iterator<Expression> iterator() {
		return new TupleIterator();
	}

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) throws Exception {
		Expression[] a = new Expression[this.values.length];
		int i = 0;
		for (Expression e : this) {
			a[i] = e.substituteTopLevelVariables(topLevel);
			i++;
		}
		return new Tuple(a);
	}

	private class TupleIterator implements Iterator<Expression> {

		private int cursor = 0;

		@Override
		public boolean hasNext() {
			return cursor < Tuple.this.values.length;
		}

		@Override
		public Expression next() {
			if (this.hasNext()) {
				Expression e = Tuple.this.values[this.cursor];
				cursor++;
				return e;
			}
			throw new NoSuchElementException();
		}

		@Override
		public void remove() {
			throw new UnsupportedOperationException();

		}

	}

	@Override
	public String toClojureCode() throws Exception {
		StringBuilder s = new StringBuilder();
		s.append('[');
		
		Iterator<Expression> i = this.iterator();
		while(i.hasNext()){
			Expression e = i.next();
			s.append(e.toClojureCode());
			if(i.hasNext()){
				s.append(' ');
			}
		}
		s.append(']');
		return s.toString();
	}
}
