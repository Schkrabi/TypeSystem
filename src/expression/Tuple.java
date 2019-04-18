package expression;

import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.TreeMap;

import types.Type;
import types.TypeTuple;
import util.AppendableException;
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
		Expression[] vls = new Expression[this.values.length];
		
		for(int i = 0; i < this.values.length; i++) {
			vls[i] = this.values[i].interpret(env);
		}
		Tuple t = new Tuple(vls);
		return t;
		
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
	public Map<Expression, Type> infer(Environment env) throws AppendableException {
		try {
			Map<Expression, Type> hyp = new TreeMap<Expression, Type>();
			if(this.typeHypothesis == null) {
				this.typeHypothesis = new TreeMap<Expression, Type>();
				
				Type types[] = new Type[this.values.length];
				for(int i = 0; i < this.values.length; i++) {
					this.typeHypothesis.putAll(this.values[i].infer(env));
					types[i] = this.typeHypothesis.get(this.values[i]);
				}
				this.typeHypothesis.put(this, new TypeTuple(types).quantifyUnconstrainedVariables());
			}
			hyp.putAll(this.typeHypothesis);
			return hyp;
		}catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
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
	
	@Override
	public int compareTo(Expression other) {
		if(other instanceof Tuple) {
			Tuple o = (Tuple)other;
			
			int c = (int)Math.signum(this.values.length - o.values.length);
			if(c != 0)
				return c;
			
			Iterator<Expression> i = this.iterator();
			Iterator<Expression> j = o.iterator();
			
			while(i.hasNext() && j.hasNext()) {
				Expression e = i.next();
				Expression f = j.next();
				
				c = e.compareTo(f);
				if(c != 0)
					return c;
			}
			return 0;
		}
		return super.compareTo(other);
	}
}
