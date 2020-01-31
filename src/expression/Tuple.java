package expression;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Vector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import types.Substitution;
import types.Type;
import types.TypeTuple;
import types.TypeVariable;
import util.AppendableException;
import util.NameGenerator;
import util.Pair;
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
	private final Vector<Expression> values;

	public Tuple(Collection<? extends Expression> values) {
		this.values = new Vector<Expression>(values);
	}

	/**
	 * Empty tuple constructornew Vector<Expression>(
	 */
	private Tuple() {
		this.values = new Vector<Expression>();
	}

	/**
	 * Gets expression on tuple index
	 * 
	 * @param index searched index
	 * @return element on given index
	 */
	public Expression get(int index) {
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

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		List<Expression> vls = new LinkedList<Expression>();

		for (Expression e : this) {
			vls.add(e.interpret(env));
		}
		return new Tuple(vls);
	}

	@Override
	public String toString() {
		StringBuilder s = new StringBuilder("[");
		Iterator<Expression> i = this.iterator();
		while (i.hasNext()) {
			Expression e = i.next();
			s.append(e.toString());
			if (i.hasNext()) {
				s.append(" ");
			}
		}
		s.append("]");
		return s.toString();
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		try {
			Substitution s = Substitution.EMPTY;
			List<Type> types = new LinkedList<Type>();

			for (Expression e : this) {
				Pair<Type, Substitution> infered = e.infer(env);
				types.add(infered.first);
				s = s.union(infered.second);
			}
			return new Pair<Type, Substitution>(new TypeTuple(types), s);

		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	@Override
	public Iterator<Expression> iterator() {
		return this.values.iterator();
	}

	@Override
	public String toClojureCode() throws AppendableException {
		return this.toClojureCode(
				new TypeTuple(
						this.stream().map(x -> new TypeVariable(NameGenerator.next())).collect(Collectors.toList())),
				Environment.topLevelEnvironment);
	}

	@Override
	protected String toClojureCode(Type expectedType, Environment env) throws AppendableException {
		StringBuilder s = new StringBuilder();
		s.append('[');

		Iterator<Expression> i = this.iterator();
		Iterator<Type> j = ((TypeTuple) expectedType).iterator();
		while (i.hasNext()) {
			Expression e = i.next();
			Type t = j.next();
			s.append(e.toClojureCode(t, env));
			if (i.hasNext()) {
				s.append(' ');
			}
		}
		s.append(']');
		return s.toString();
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof Tuple) {
			Tuple o = (Tuple) other;

			int c = (int) Math.signum(this.values.size() - o.values.size());
			if (c != 0)
				return c;

			Iterator<Expression> i = this.iterator();
			Iterator<Expression> j = o.iterator();

			while (i.hasNext()) {
				Expression e = i.next();
				Expression f = j.next();

				c = e.compareTo(f);
				if (c != 0)
					return c;
			}
			return 0;
		}
		return super.compareTo(other);
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof Tuple) {
			return this.values.equals(((Tuple) other).values);
		}
		return false;
	}

	public Stream<Expression> stream() {
		return this.values.stream();
	}

	@Override
	public int hashCode() {
		return this.values.hashCode();
	}

	/**
	 * Empty tuple expression
	 */
	public static final Tuple EMPTY_TUPLE = new Tuple();
}
