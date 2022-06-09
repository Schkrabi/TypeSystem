package velka.core.abstraction;

import velka.types.RepresentationOr;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeSetDoesNotUnifyException;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import velka.core.expression.Expression;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.util.AppendableException;
import velka.util.Pair;
import velka.util.ThrowingBinaryOperator;

/**
 * Expression for interpreted function with various implementations
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class ExtendedFunction extends ExtendedLambda {

	public final Environment creationEnvironment;

	private ExtendedFunction(Collection<Function> implementations, Expression rankingFunction,
			Environment createdEnvironment) {
		super(implementations, rankingFunction);
		creationEnvironment = createdEnvironment;
	}
	
	/**
	 * Gets shallow copy of implementations set as Functions
	 * @return Set
	 */
	public Set<Function> getImplementationsAsFunctions(){
		return this.implementations.stream().map(x -> (Function)x).collect(Collectors.toSet());
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		try {
			Set<Pair<Type, Substitution>> s = new HashSet<Pair<Type, Substitution>>();
			for (Expression e : this.implementations) {
				Pair<Type, Substitution> p = e.infer(env, typeEnv);
				s.add(p);
			}

			return new Pair<Type, Substitution>(
					RepresentationOr.makeRepresentationOr(s.stream().map(x -> x.first).collect(Collectors.toSet())),
					s.stream().map(x -> x.second).reduce(Substitution.EMPTY,
							ThrowingBinaryOperator.wrapper((x, y) -> {
								Optional<Substitution> unifier = x.union(y);
								if(unifier.isEmpty()) {
									throw new TypeSetDoesNotUnifyException(s.stream().map(p -> p.first).collect(Collectors.toSet()));
								}
								return unifier.get();
							})));

		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof ExtendedFunction) {
			int cmp = this.creationEnvironment.compareTo(((ExtendedFunction) other).creationEnvironment);
			if (cmp != 0)
				return cmp;
		}
		return super.compareTo(other);
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof ExtendedFunction) {
			boolean envEquals = this.creationEnvironment.equals(((ExtendedFunction) other).creationEnvironment);

			if (!envEquals) {
				return false;
			}

			// Have to compare implementation by implementation to get unification of
			// argtypes
			for (Lambda l : this.implementations) {
				boolean found = false;
				for (Lambda k : ((ExtendedFunction) other).implementations) {
					if (k.equals(l)) {
						found = true;
						break;
					}
				}
				if (!found) {
					return false;
				}
			}

			for (Lambda k : ((ExtendedFunction) other).implementations) {
				boolean found = false;
				for (Lambda l : this.implementations) {
					if (l.equals(k)) {
						found = true;
						break;
					}
				}
				if (!found) {
					return false;
				}
			}

			return true;
		}
		return false;
	}

	@Override
	public String toString() {
		StringBuilder s = new StringBuilder("(ExFunctionInternal (");

		// Arguments
		Iterator<? extends Lambda> k = this.implementations.iterator();
		while (k.hasNext()) {
			Lambda f = k.next();
			s.append('(');
			s.append(f.argsType.toString().replace('[', '(').replace(']', ')'));
			s.append(' ');
			s.append(f.body.toString());
			s.append(')');
			if (k.hasNext()) {
				s.append(' ');
			}
		}

		s.append("))");

		return s.toString();
	}

	@Override
	public int hashCode() {
		return super.hashCode() * this.implementations.hashCode();
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
		return ExtendedFunction.makeExtendedFunction(implementations, createdEnvironment,
				ExtendedLambda.defaultSelectionFunction);
	}

	/**
	 * Creates new extended function
	 * 
	 * @param implementations    function implementations
	 * @param rankingFunction    ranking function used for selecting implementation
	 * @param createdEnvironment environment where function was created
	 * @return new ExtendedFunction object
	 * @throws AppendableException thrown if argument types of function does not
	 *                             unify
	 */
	public static ExtendedFunction makeExtendedFunction(Collection<Function> implementations,
			Environment createdEnvironment, Expression rankingFunction) throws AppendableException {
		Type.unifyMany(implementations.stream().map(x -> x.argsType).collect(Collectors.toSet()));
		return new ExtendedFunction(implementations, rankingFunction, createdEnvironment);
	}

	@Override
	protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
			Optional<Expression> rankingFunction) throws AppendableException {
		Abstraction a = this.selectImplementation(args, rankingFunction, env, typeEnv);
		return a.doSubstituteAndEvaluate(args, env, typeEnv, rankingFunction);
	}

	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) {
		return this;
	}
}
