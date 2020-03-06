package expression;

import interpretation.Environment;

import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.stream.Collectors;

import types.RepresentationOr;
import types.Substitution;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import util.AppendableException;
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

	private ExtendedFunction(Collection<Function> implementations, Environment createdEnvironment) {
		super(createdEnvironment);
		this.implementations = implementations.stream()
				.map(x -> new Function(x.argsType, x.args, x.body, createdEnvironment)).collect(Collectors.toSet());
	}

	@Override
	public Function getFunction(final TypeTuple realArgsType) {
		return this.implementations.stream()
				.map(impl -> new Pair<Integer, Function>(impl.argsType.tupleDistance(realArgsType), impl))
				.reduce(new Pair<Integer, Function>(Integer.MAX_VALUE, null), (p1, p2) -> {
					if (p1.first < p2.first)
						return p1;
					else
						return p2;
				}).second;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		try {
			Set<Pair<Type, Substitution>> s = this.implementations.stream()
					.map(ThrowingFunction.wrapper(x -> x.infer(env))).collect(Collectors.toSet());

			return new Pair<Type, Substitution>(
					RepresentationOr.makeRepresentationOr(s.stream().map(x -> x.first).collect(Collectors.toSet())),
					s.stream().map(x -> x.second).reduce(Substitution.EMPTY,
							ThrowingBinaryOperator.wrapper((x, y) -> x.union(y))));

		} catch (AppendableException e) {
			e.appendMessage("in " + this);
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
			int cmp = this.creationEnvironment.compareTo(((ExtendedFunction) other).creationEnvironment);
			if (cmp != 0)
				return cmp;

			for (Function f : this.implementations) {
				if (!((ExtendedFunction) other).implementations.contains(f)) {
					return 1;
				}
			}
			for (Function f : ((ExtendedFunction) other).implementations) {
				if (!this.implementations.contains(f)) {
					return -1;
				}
			}
			return 0;
		}
		return super.compareTo(other);
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof ExtendedFunction) {
			return this.implementations.equals(((ExtendedFunction) other).implementations);
		}
		return false;
	}

	@Override
	public String toString() {
		StringBuilder s = new StringBuilder("(ExFunctionInternal (");

		// Arguments
		Iterator<Function> k = this.implementations.iterator();
		while (k.hasNext()) {
			Function f = k.next();
			s.append('(');
			s.append(f.argsType.toString().replace('[', '(').replace(']', ')'));
			s.append(' ');
			s.append(f.body.toString());
			s.append(')');
			if (k.hasNext()) {
				s.append(' ');
			}
		}

		s.append(')');

		return s.toString();
	}

	@Override
	public int hashCode() {
		return super.hashCode() * this.implementations.hashCode();
	}

	@Override
	public TypeArrow getFunctionTypeWithRepresentations(TypeTuple argTypes, Environment env)
			throws AppendableException {
		return this.getFunction(argTypes).getFunctionTypeWithRepresentations(argTypes, env);
	}

	/**
	 * Creates new extended function
	 * 
	 * @param implementations    function implementations
	 * @param createdEnvironment environment where function was created
	 * @return new ExtendedFunction object
	 * @throws AppendableException thrown if argument types of function does not
	 *                             unify
	 */
	public static ExtendedFunction makeExtendedFunction(Collection<Function> implementations,
			Environment createdEnvironment) throws AppendableException {
		Type.unifyMany(implementations.stream().map(x -> x.argsType).collect(Collectors.toSet()));
		return new ExtendedFunction(implementations, createdEnvironment);
	}
}
