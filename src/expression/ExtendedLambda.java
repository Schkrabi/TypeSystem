package expression;

import java.util.Collection;
import java.util.Comparator;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import interpretation.Environment;
import types.Substitution;
import types.Type;
import types.TypeTuple;
import types.TypeVariable;
import util.AppendableException;
import util.NameGenerator;
import util.Pair;
import util.ThrowingBinaryOperator;
import util.ThrowingFunction;

/**
 * Extended lambda expression allowing for the different implementation of body
 * based on the arguments representation
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public class ExtendedLambda extends MetaLambda {

	/*
	 * TODO list * strenghten syntax of elambda as follows (elambda ((type1 arg1)
	 * (type2 arg2) ...) ((typeRep11 typeRep12 ...) impl1) ((typeRep21 typeRep22
	 * ...) impl2)) * argument list will be mandatory fully typed with TypeConcrete
	 * (not TypeRepresentation !) * implementation argument type will have be typed
	 * with TypeRepresentation * default implementation will be deprecated
	 * 
	 * * Maybe improve syntax of type in typed lambda to be able to use "->" and
	 * "[]" (functions and tuples) (not critical)
	 */

	/**
	 * Various implementations of this function
	 */
	private final Set<Lambda> implementations;

	/**
	 * Type of arguments
	 */
	public final TypeTuple argsType;

	public ExtendedLambda(TypeTuple argsType, Collection<Lambda> implementations) {
		this.argsType = argsType;
		this.implementations = new TreeSet<Lambda>(implementations);
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		return new ExtendedFunction(this.argsType,
				this.implementations.stream().map(x -> (Function) x.interpret(env)).collect(Collectors.toSet()), env);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		final TypeVariable v = new TypeVariable(NameGenerator.next());
		try {
			// TODO rewrite with argsType
			return this.implementations.stream().map(ThrowingFunction.wrapper(x -> x.infer(env))).reduce(
					new Pair<Type, Substitution>(v, new Substitution()), ThrowingBinaryOperator.wrapper((x, y) -> {
						Substitution s = Type.unify(x.first, y.first).get();
						s = s.compose(x.second).compose(y.second);
						return new Pair<Type, Substitution>(v.apply(s), s);
					}));
		} catch (RuntimeException e) {
			if (e.getCause() instanceof AppendableException) {
				AppendableException ae = (AppendableException) e.getCause();
				ae.appendMessage("in " + this);
				throw ae;
			}
			throw e;
		}
	}

	/**
	 * Returns the priority queue with natural ordering of the optimized
	 * implementations
	 * 
	 * @return priority queue of ImplContainers
	 */
	public PriorityQueue<Lambda> getSortedImplementations() {
		PriorityQueue<Lambda> q = new PriorityQueue<Lambda>();
		q.addAll(this.implementations);
		return q;
	}

	/**
	 * Returns the priority queue with ordering of the optimized implementations
	 * given by comparator
	 * 
	 * @param c comparator determining the ordering of the implementations
	 * @return priority queue of ImplContainers
	 */
	public PriorityQueue<Lambda> getSortedImplementations(Comparator<? super Lambda> c) {
		PriorityQueue<Lambda> q = new PriorityQueue<Lambda>(c);
		q.addAll(this.implementations);
		return q;
	}

	@Override
	public String toString() {
		// TODO
		return "";
	}

	@Override
	public String toClojureCode() throws AppendableException {
		// TODO
		return "";
	}

	@Override
	public Lambda getLambda(Comparator<? super Lambda> c) {
		PriorityQueue<Lambda> queue = this.getSortedImplementations(c);
		return queue.peek();
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof ExtendedLambda) {
			ExtendedLambda o = (ExtendedLambda) other;

			int cmp = this.argsType.compareTo(o.argsType);
			if (cmp != 0)
				return cmp;

			for (Lambda l : this.implementations) {
				if (!o.implementations.contains(l))
					return 1;
			}
			for (Lambda l : o.implementations) {
				if (!this.implementations.contains(l))
					return -1;
			}
			return 0;
		}
		return super.compareTo(other);
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof ExtendedLambda) {
			return this.argsType.equals(((ExtendedLambda) other).argsType)
					&& this.implementations.equals(((ExtendedLambda) other).implementations);
		}
		return false;
	}
}
