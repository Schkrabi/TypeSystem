package expression;

import interpretation.Environment;

import java.util.Comparator;
import java.util.PriorityQueue;
import java.util.Set;

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
 * Expression for interpreted function with various implementations
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class ExtendedFunction extends MetaFunction {

	/**
	 * Implementations of the function
	 */
	private final Set<Function> implementations;

	/**
	 * Type of arguments
	 */
	public final TypeTuple argsType;

	public ExtendedFunction(TypeTuple argsType, Set<Function> implementations, Environment createdEnvironment) {
		super(createdEnvironment);
		this.argsType = argsType;
		this.implementations = implementations;
	}

	@Override
	public Function getFunction(Comparator<? super Function> c) {
		return this.getSortedImplementations(c).peek();
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		// TODO
		final Environment crEnv = this.creationEnvironment;
		final TypeVariable v = new TypeVariable(NameGenerator.next());
		try {
			return this.implementations.stream().map(ThrowingFunction.wrapper(x -> x.infer(crEnv))).reduce(
					new Pair<Type, Substitution>(v, new Substitution()), ThrowingBinaryOperator.wrapper((x, y) -> {
						Substitution s = Type.unify(x.first, y.first).get().compose(x.second).compose(y.second);
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

	public PriorityQueue<Function> getSortedImplementations(Comparator<? super Function> c) {
		PriorityQueue<Function> q = new PriorityQueue<Function>(c);
		q.addAll(this.implementations);
		return q;
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof ExtendedFunction) {
			int cmp = this.argsType.compareTo(((ExtendedFunction) other).argsType);
			if (cmp != 0)
				return cmp;
			cmp = this.creationEnvironment.compareTo(((ExtendedFunction) other).creationEnvironment);
			if (cmp != 0)
				return cmp;

			for (Function f : this.implementations) {
				if (((ExtendedFunction) other).implementations.contains(f))
					return 1;
			}
			for (Function f : ((ExtendedFunction) other).implementations) {
				if (this.implementations.contains(f))
					return -1;
			}
			return 0;
		}
		return super.compareTo(other);
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof ExtendedFunction) {
			return this.argsType.equals(((ExtendedFunction) other).argsType)
					&& this.implementations.equals(((ExtendedFunction) other).implementations) && super.equals(other);
		}
		return false;
	}
}
