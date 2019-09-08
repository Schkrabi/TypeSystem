package expression;

import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import interpretation.Environment;
import types.Substitution;
import types.Type;
import types.TypeArrow;
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
			try {
				Pair<Type, Substitution> p = this.implementations.stream()
						.map(ThrowingFunction.wrapper(x -> x.infer(env)))
						.reduce(new Pair<Type, Substitution>(v, Substitution.EMPTY),
								ThrowingBinaryOperator.wrapper((x, y) -> {
									Substitution s = Type.unify(x.first, y.first);
									s = s.union(x.second).union(y.second);
									return new Pair<Type, Substitution>(v.apply(s), s);
								}));

				return new Pair<Type, Substitution>(p.first.removeRepresentationInfo(),
						p.second.union(Type.unify(((TypeArrow) (p.first)).ltype, this.argsType)));
			} catch (RuntimeException e) {
				AppendableException ae = (AppendableException) e.getCause();
				throw ae;
			}
		} catch (AppendableException e) {
			e.appendMessage("in " + this);
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
	 * @param c
	 *            comparator determining the ordering of the implementations
	 * @return priority queue of ImplContainers
	 */
	public PriorityQueue<Lambda> getSortedImplementations(Comparator<? super Lambda> c) {
		PriorityQueue<Lambda> q = new PriorityQueue<Lambda>(c);
		q.addAll(this.implementations);
		return q;
	}

	@Override
	public String toString() {
		StringBuilder s = new StringBuilder("(elambda (");
		Lambda sample = this.implementations.stream().findAny().get();

		// Arguments
		Iterator<Expression> i = sample.args.iterator();
		Iterator<Type> j = this.argsType.iterator();
		while (i.hasNext()) {
			Expression e = i.next();
			Type t = j.next();
			s.append('(');
			s.append(t.toString());
			s.append(' ');
			s.append(e.toString());
			s.append(')');
		}
		s.append(") ");

		Iterator<Lambda> k = this.implementations.iterator();
		while(k.hasNext()) {
			Lambda l = k.next();
			s.append('(');
			s.append(l.argsType.toString().replace('[', '(').replace(']', ')'));
			s.append(' ');
			s.append(l.body.toString());
			s.append(')');
			if(k.hasNext()) {
				s.append(' ');
			}
		}

		s.append(')');

		return s.toString();
	}

	@Override
	public String toClojureCode() throws AppendableException {
		// TODO
		return "";
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
	
	@Override
	public int hashCode() {
		return this.argsType.hashCode() * this.implementations.hashCode();
	}
}
