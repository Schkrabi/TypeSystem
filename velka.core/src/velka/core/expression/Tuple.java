package velka.core.expression;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.Vector;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import velka.util.AppendableException;
import velka.util.ClojureHelper;
import velka.util.Pair;
import velka.util.ThrowingFunction;
import velka.core.exceptions.ConversionException;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.types.Substitution;
import velka.types.SubstitutionsCannotBeMergedException;
import velka.types.Type;
import velka.types.TypeTuple;

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
	
	public Tuple(Stream<? extends Expression> values) {
		this.values = new Vector<Expression>(values.collect(Collectors.toList()));
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
				
				Optional<Substitution> mergeRslt = s.merge(infered.second);
				
				if(mergeRslt.isEmpty()) {
					throw new SubstitutionsCannotBeMergedException(s, infered.second);
				}
				
				s = mergeRslt.get();
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
		List<String> members = null;
		try {
			members = this.stream().map(ThrowingFunction.wrapper(e -> e.toClojureCode(env, typeEnv))).collect(Collectors.toList());
		}catch(RuntimeException re) {
			if(re.getCause() instanceof AppendableException) {
				AppendableException ae = (AppendableException)re.getCause();
				ae.appendMessage(" in " + this.toString());
				throw ae;
			}
			throw re;
		}
		
		return ClojureHelper.tupleHelper(members);	
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
	 * Converts tuple into list
	 * @return list
	 */
	public List<Expression> toList(){
		return this.stream().collect(Collectors.toList());
	}
	
	/**
	 * Tuple collector
	 */
	public static final Collector<Expression, ArrayList<Expression>, Tuple> toTuple =
			Collector.of(
					ArrayList<Expression>::new,
					ArrayList<Expression>::add,
					(first, second) -> { first.addAll(second); return first; },
					(ArrayList<Expression> l) -> new Tuple(l));

	/**
	 * Empty tuple expression
	 */
	public static final Tuple EMPTY_TUPLE = new Tuple();
	
	/**
	 * Clojure code for empty tuple
	 */
	public static final String EMPTY_TUPLE_CLOJURE = ClojureHelper.tupleHelper();

	@Override
	public Expression doConvert(Type from, Type to, Environment env, TypeEnvironment typeEnv) throws AppendableException {
		if(!(to instanceof TypeTuple)) {
			throw new ConversionException(to, this);
		}
		TypeTuple to_typeTuple = (TypeTuple)to;
		
		if(to_typeTuple.size() != this.size()) {
			throw new ConversionException(to, this);
		}
		
		List<Expression> l = new LinkedList<Expression>();
		Iterator<Type> it_to = to_typeTuple.iterator();
		Iterator<Expression> it = this.iterator();
		while(it.hasNext()) {
			Expression e = it.next();
			Type t = it_to.next();
			Expression c = e.convert(t, env, typeEnv);
			l.add(c);
		}
		return new Tuple(l);
	}
}
