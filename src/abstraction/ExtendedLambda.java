package abstraction;

import java.util.Collection;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import expression.Expression;
import expression.Tuple;
import interpretation.Environment;
import types.RepresentationOr;
import types.Substitution;
import types.Type;
import types.TypeTuple;
import util.AppendableException;
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
public class ExtendedLambda extends Abstraction {

	/**
	 * Various implementations of this function
	 */
	protected final Set<? extends Lambda> implementations;

	protected ExtendedLambda(Collection<? extends Lambda> implementations) {
		this.implementations = new TreeSet<Lambda>(implementations);
	}
	
	protected Lambda getMostFitLambda(final TypeTuple argsType) {
		return this.implementations.stream()
				.map(impl -> new Pair<Integer, Lambda>(impl.argsType.tupleDistance(argsType), impl))
				.reduce(new Pair<Integer, Lambda>(Integer.MAX_VALUE, null), (p1, p2) -> {
					if (p1.first < p2.first)
						return p1;
					else
						return p2;
				}).second;						
	}

	@Override
	public Expression interpret(final Environment env) throws AppendableException {
		Set<Function> fs = null;
		try {
			fs = this.implementations.stream().map(ThrowingFunction.wrapper(x -> (Function)x.interpret(env))).collect(Collectors.toSet());
		}catch(RuntimeException re) {
			AppendableException e = (AppendableException)re.getCause();
			throw e;
		}
		
		return ExtendedFunction.makeExtendedFunction(fs, env);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		try {
			Set<Pair<Type, Substitution>> s = null;
			try {
				s = this.implementations.stream()
					.map(ThrowingFunction.wrapper(x -> x.infer(env))).collect(Collectors.toSet());
			} catch(RuntimeException re) {
				AppendableException e = (AppendableException)re.getCause();
				throw e;
			}

			return new Pair<Type, Substitution>(
					RepresentationOr.makeRepresentationOr(s.stream().map(x -> x.first).collect(Collectors.toSet())),
					s.stream().map(x -> x.second).reduce(Substitution.EMPTY,
							ThrowingBinaryOperator.wrapper((x, y) -> x.union(y))));

		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	@Override
	public String toString() {
		StringBuilder s = new StringBuilder("(elambda (");

		Iterator<? extends Lambda> k = this.implementations.iterator();
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

	@Override
	protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
		ExtendedFunction f = (ExtendedFunction)this.interpret(env);
		return f.doSubstituteAndEvaluate(args, env);
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

	@Override
	protected String implementationsToClojure(Environment env) throws AppendableException {
		StringBuilder sb = new StringBuilder();
		
		Iterator<? extends Lambda> i = this.implementations.iterator();
		while(i.hasNext()) {
			Lambda implementation = i.next();
			sb.append(implementation.toClojureFn(env));
			if(i.hasNext()) {
				sb.append(" ");
			}
		}
		
		return sb.toString();
	}
}
