package velka.lang.abstraction;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import velka.lang.application.AbstractionApplication;
import velka.lang.expression.Expression;
import velka.lang.expression.Tuple;
import velka.lang.expression.TypeSymbol;
import velka.lang.interpretation.ClojureCodeGenerator;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.langbase.ListNative;
import velka.lang.literal.LitInteger;
import velka.lang.types.RepresentationOr;
import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeTuple;
import velka.lang.util.AppendableException;
import velka.lang.util.Pair;
import velka.lang.util.ThrowingBinaryOperator;
import velka.lang.util.ThrowingFunction;

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

	/**
	 * Ranking function for this extended lambda
	 */
	public final Expression rankingFunction;

	protected ExtendedLambda(Collection<? extends Lambda> implementations, Expression rankingFunction) {
		this.implementations = new TreeSet<Lambda>(implementations);
		this.rankingFunction = rankingFunction;
	}

	@Override
	public Expression interpret(final Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Set<Function> fs = null;
		try {
			fs = this.implementations.stream().map(ThrowingFunction.wrapper(x -> (Function) x.interpret(env, typeEnv)))
					.collect(Collectors.toSet());
		} catch (RuntimeException re) {
			AppendableException e = (AppendableException) re.getCause();
			throw e;
		}

		return ExtendedFunction.makeExtendedFunction(fs, env, this.rankingFunction);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		try {
			Set<Pair<Type, Substitution>> s = null;
			try {
				s = this.implementations.stream().map(ThrowingFunction.wrapper(x -> x.infer(env, typeEnv)))
						.collect(Collectors.toSet());
			} catch (RuntimeException re) {
				AppendableException e = (AppendableException) re.getCause();
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
	protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
			Optional<Expression> rankingFunction) throws AppendableException {
		ExtendedFunction f = (ExtendedFunction) this.interpret(env, typeEnv);
		return f.doSubstituteAndEvaluate(args, env, typeEnv, rankingFunction);
	}
	
	/**
	 * Convenience constructor for extended lambda
	 * @param impls implementations
	 * @return new extended lambda instance
	 * @throws AppendableException if implementations arguments does not unify
	 */
	public static ExtendedLambda makeExtendedLambda(Lambda... impls) throws AppendableException {
		return ExtendedLambda.makeExtendedLambda(Arrays.asList(impls));
	}

	/**
	 * Creates new Extended lambda expression
	 * 
	 * @param implementations implementations of extended lambda
	 * @return new ExtendedLambda object
	 * @throws AppendableException thrown if any of the argument types does not
	 *                             unify
	 */
	public static ExtendedLambda makeExtendedLambda(Collection<Lambda> implementations) throws AppendableException {
		return ExtendedLambda.makeExtendedLambda(implementations, AbstractionApplication.defaultRanking);
	}

	/**
	 * Creates new Extended lambda expression
	 * 
	 * @param implementations implementations of extended lambda
	 * @param rankingFunction ranking function used for selecting implementation
	 * @return new ExtendedLambda object
	 * @throws AppendableException thrown if any of the argument types does not
	 *                             unify
	 */
	public static ExtendedLambda makeExtendedLambda(Collection<Lambda> implementations, Expression rankingFunction)
			throws AppendableException {
		Type.unifyMany(implementations.stream().map(x -> x.argsType).collect(Collectors.toSet()));
		return new ExtendedLambda(implementations, rankingFunction);
	}

	@Override
	protected String implementationsToClojure(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		StringBuilder sb = new StringBuilder();

		sb.append("(let [impls #{");
		for (Lambda l : this.implementations) {
			sb.append(l.toClojureFn(env, typeEnv));
			sb.append("\n");
		}
		sb.append("}]");
		sb.append("(fn ");

		sb.append("([args] (");
		sb.append(ClojureCodeGenerator.selectImplementationClojureSymbol);
		sb.append(" ");
		sb.append(this.rankingFunction.toClojureCode(env, typeEnv));
		sb.append(" args impls))\n");

		sb.append("([args ranking-fn] (");
		sb.append(ClojureCodeGenerator.selectImplementationClojureSymbol);
		sb.append(" ranking-fn args impls))))");

		return sb.toString();
	}

	@Override
	public Abstraction selectImplementation(Tuple args, Optional<Expression> rankingFunction, Environment env,
			TypeEnvironment typeEnv) throws AppendableException {
		TypeTuple argsType = (TypeTuple) args.infer(env, typeEnv).first;
		final Expression realArgsList = ListNative.tupleToListNative(
				new Tuple(argsType.stream().map(t -> new TypeSymbol(t)).collect(Collectors.toList())));

		try {
			Abstraction abst = this.implementations.stream().map(ThrowingFunction.wrapper(implementation -> {
				Expression formalArgsList = ListNative.tupleToListNative(new Tuple(
						implementation.argsType.stream().map(t -> new TypeSymbol(t)).collect(Collectors.toList())));

				AbstractionApplication appl = new AbstractionApplication(
						rankingFunction.isPresent() ? rankingFunction.get() : this.rankingFunction,
						new Tuple(formalArgsList, realArgsList));
				Expression rankingResult = appl.interpret(env, typeEnv);
				if (!(rankingResult instanceof LitInteger)) {
					throw new AppendableException("Badly formed ranking function " + rankingFunction.toString());
				}
				long result = ((LitInteger) rankingResult).value;
				return new Pair<Long, Abstraction>(result, implementation);
			})).reduce(new Pair<Long, Abstraction>(Long.MAX_VALUE, null), (p1, p2) -> {
				if (p1.first < p2.first) {
					return p1;
				} else {
					return p2;
				}
			}).second;

			return abst;
		} catch (RuntimeException re) {
			if (re.getCause() instanceof AppendableException) {
				AppendableException e = (AppendableException) re.getCause();
				throw e;
			}
			throw re;
		}
	}
}
