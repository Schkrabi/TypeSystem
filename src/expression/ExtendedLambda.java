package expression;

import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import interpretation.Environment;
import types.RepresentationOr;
import types.Substitution;
import types.Type;
import types.TypeArrow;
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

	/**
	 * Various implementations of this function
	 */
	private final Set<Lambda> implementations;

	private ExtendedLambda(Collection<Lambda> implementations) {
		this.implementations = new TreeSet<Lambda>(implementations);
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		return ExtendedFunction.makeExtendedFunction(
				this.implementations.stream().map(x -> (Function) x.interpret(env)).collect(Collectors.toSet()), env);
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
		StringBuilder s = new StringBuilder("(elambda (");

		Iterator<Lambda> k = this.implementations.iterator();
		while (k.hasNext()) {
			Lambda l = k.next();
			s.append('(');
			s.append(l.argsType.toString().replace('[', '(').replace(']', ')'));
			s.append(' ');
			s.append(l.body.toString());
			s.append(')');
			if (k.hasNext()) {
				s.append(' ');
			}
		}

		s.append(')');

		return s.toString();
	}

	@Override
	public String toClojureCode() throws AppendableException {
		return this.toClojureCode(
				new TypeArrow(new TypeVariable(NameGenerator.next()), new TypeVariable(NameGenerator.next())),
				Environment.topLevelEnvironment);
	}

	@Override
	protected String toClojureCode(Type expectedType, Environment env) throws AppendableException {
		if (!expectedType.isApplicableType() && !(expectedType instanceof TypeVariable)) {
			throw new AppendableException(
					"Unexpected argument " + expectedType + "in ExtendedLambda.toClojureCode(expectedType)");
		}

		try {			
			if(expectedType instanceof TypeArrow){
				final TypeArrow type = (TypeArrow) expectedType;
				StringBuilder s = new StringBuilder("`(");
	
				Iterator<String> i = this.implementations.stream()
						.map(ThrowingFunction
								.wrapper(x -> "[" + x.argsType.toClojure() + " ~" + x.toClojureFn(type, env) + "]"))
						.collect(Collectors.toList()).iterator();
	
				while (i.hasNext()) {
					String str = i.next();
					s.append(str);
					if (i.hasNext()) {
						s.append(" ");
					}
				}
				s.append(")");
				return s.toString();
			}
			else if (expectedType instanceof RepresentationOr) {
				RepresentationOr type = (RepresentationOr) expectedType;
				StringBuilder s = new StringBuilder("`(");
				
				Iterator<Lambda> i = this.implementations.iterator();
				
				while(i.hasNext()) {
					Lambda l = i.next();
					Type t = type.getRepresentations().stream().filter(x -> ((TypeArrow)x).ltype.equals(l.argsType)).findAny().get();
					s.append("[" + l.argsType.toClojure() + " ~" + l.toClojureFn(t, env) + "]");
					if (i.hasNext()) {
						s.append(" ");
					}
				}
				s.append(")");
				
				return s.toString();
				
			}
			else {
				throw new AppendableException("Unexcpected expected type: " + expectedType);
			}
		} catch (RuntimeException re) {
			AppendableException e = (AppendableException) re.getCause();
			throw e;
		}
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof ExtendedLambda) {
			ExtendedLambda o = (ExtendedLambda) other;

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
			return this.implementations.equals(((ExtendedLambda) other).implementations);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.implementations.hashCode();
	}

	/**
	 * Creates new Extended lambda exppression
	 * 
	 * @param implementations implementations of extended lambdas
	 * @return new ExtendedLambda object
	 * @throws AppendableException thrown if any of the argument types does not
	 *                             unify
	 */
	public static ExtendedLambda makeExtendedLambda(Collection<Lambda> implementations) throws AppendableException {
		Type.unifyMany(implementations.stream().map(x -> x.argsType).collect(Collectors.toSet()));
		return new ExtendedLambda(implementations);
	}
}
