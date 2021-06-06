package velka.lang.expression;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.Vector;
import java.util.stream.Stream;

import velka.lang.types.Substitution;
import velka.lang.types.SubstitutionsCannotBeMergedException;
import velka.lang.types.Type;
import velka.lang.types.TypeTuple;
import velka.lang.util.AppendableException;
import velka.lang.util.Pair;
import velka.lang.interpretation.ClojureCodeGenerator;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;

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
	
	public Tuple(Expression ...exprs) {
		this.values = new Vector<Expression>(Arrays.asList(exprs));
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

	/**
	 * Reverses this tuple
	 * 
	 * @return new tuple object reversed
	 */
	public Tuple reverse() {
		List<Expression> l = new LinkedList<Expression>();
		for (int i = this.values.size() - 1; i >= 0; i--) {
			l.add(this.values.get(i));
		}
		return new Tuple(l);
	}

	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		List<Expression> vls = new LinkedList<Expression>();

		for (Expression e : this) {
			vls.add(e.interpret(env, typeEnv));
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
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		try {
			Substitution s = Substitution.EMPTY;
			List<Type> types = new LinkedList<Type>();

			for (Expression e : this) {
				Pair<Type, Substitution> infered = e.infer(env, typeEnv);
				types.add(infered.first);
				
				Optional<Substitution> opt = s.union(infered.second);
				if(opt.isEmpty()) {
					throw new SubstitutionsCannotBeMergedException(s, infered.second);
				}
				
				s = opt.get();
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
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		StringBuilder s = new StringBuilder();

		s.append("(let [tuple ");

		s.append("[");

		Iterator<Expression> i = this.iterator();
		while (i.hasNext()) {
			Expression e = i.next();
			s.append(e.toClojureCode(env, typeEnv));
			if (i.hasNext()) {
				s.append(' ');
			}
		}
		s.append("]] ");
		
		s.append(ClojureCodeGenerator.addTypeMetaInfo_str("tuple", 
				"(velka.lang.types.TypeTuple. (map " + ClojureCodeGenerator.getTypeClojureSymbol + " tuple))"));
		s.append(")");
		

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
