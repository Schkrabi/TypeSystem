package expression;

import java.util.Comparator;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.TreeSet;

import interpretation.Environment;
import types.ForallType;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import types.TypeVariable;
import util.ImplContainer;

/**
 * Extended lambda expression allowing for the different implementation of body
 * based on the arguments representation
 * 
 * @author schkrabi
 * 
 */
public class ExtendedLambda extends Expression {

	/**
	 * Formal arguments (names) of the lambda expression
	 */
	public final Tuple args;

	/**
	 * Basic fallback implementation of the lambda body
	 */
	public final Expression defaultImplementation;

	/**
	 * Alternative implementations of the lambda body associated with the
	 * argument representation types
	 */
	public final Set<ImplContainer> implementations;

	public ExtendedLambda(Tuple args, Expression expr) {
		this.args = args;
		this.implementations = new TreeSet<ImplContainer>();
		this.defaultImplementation = expr;
	}

	public ExtendedLambda(Tuple args, Expression defaultImplementation,
			Set<ImplContainer> specificImplementations) {
		this.args = args;
		this.implementations = new TreeSet<ImplContainer>();
		this.implementations.addAll(specificImplementations);
		this.defaultImplementation = defaultImplementation;
	}

	/**
	 * Backward compatibility constructor
	 * 
	 * @param args
	 * @param defaultImplementation
	 * @param specificImplementations
	 */
	public ExtendedLambda(Tuple args, Expression defaultImplementation,
			Map<TypeTuple, Expression> specificImplementations) {
		this.args = args;
		this.implementations = new TreeSet<ImplContainer>();
		for (Map.Entry<TypeTuple, Expression> e : specificImplementations
				.entrySet()) {
			this.implementations
					.add(new ImplContainer(e.getKey(), e.getValue()));
		}
		this.defaultImplementation = defaultImplementation;
	}

	@Deprecated
	public Expression getDefaultUmplementation() {
		return this.defaultImplementation;
	}

	@Deprecated
	public Expression getImplementation(TypeTuple prefferedType) {
		for (ImplContainer c : this.implementations) {
			if (c.typeSpec.equals(prefferedType)) {
				return c.implementation;
			}
		}
		return null;
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		return this;
	}

	@Override
	public Type infer() throws Exception {
		Type argsType = this.args.infer();
		Type defImplType = this.defaultImplementation.infer();

		for (ImplContainer c : this.implementations) {
			TypeTuple targs = c.typeSpec;
			Type timpl = c.implementation.infer();

			if (targs == TypeTuple.EMPTY_TUPLE) {
				continue;
			}

			if (!Type.unify(targs, argsType)) {
				throw new Exception(
						"Bad extended lambda specialized arguments of type "
								+ targs.getRep().toString()
								+ " do not unify with default arguments "
								+ argsType.getRep().toString());
			}

			if (!Type.unify(defImplType, timpl)) {
				throw new Exception(
						"Bad extended lambda specialized implementation "
								+ c.implementation.toString() + "of type "
								+ timpl.toString()
								+ "do not unify with default implementation "
								+ this.defaultImplementation.toString()
								+ " of type " + defImplType.toString());
			}
		}

		Type t = new TypeArrow(argsType, defImplType);

		for (TypeVariable v : t.getUnconstrainedVariables()) {
			t = new ForallType(v, t);
		}

		this.setType(t);

		return t;
	}

	/**
	 * Returns the priority queue with natural ordering of the optimized
	 * implementations
	 * 
	 * @return priority queue of ImplContainers
	 */
	public PriorityQueue<ImplContainer> getSortedImplementations() {
		PriorityQueue<ImplContainer> q = new PriorityQueue<ImplContainer>();
		q.addAll(this.implementations);
		return q;
	}

	/**
	 * Returns the priority queue with ordering of the optimized implementations
	 * given by comparator
	 * 
	 * @param c
	 *            comparator determining the ordering of the implementations
	 * @return priority queue of ImplContainers
	 */
	public PriorityQueue<ImplContainer> getSortedImplementations(
			Comparator<? super ImplContainer> c) {
		PriorityQueue<ImplContainer> q = new PriorityQueue<ImplContainer>(c);
		q.addAll(this.implementations);
		return q;
	}

}
