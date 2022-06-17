package velka.core.abstraction;

import velka.types.RepresentationOr;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeSetDoesNotUnifyException;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
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

	protected ExtendedFunction(Map<Function, Expression> implementations, Expression rankingFunction,
			Environment createdEnvironment) {
		super(implementations, rankingFunction);
		creationEnvironment = createdEnvironment;
	}
	
	protected ExtendedFunction(Map<Function, Expression> implementations, Environment createdEnvironment) {
		super(implementations);
		this.creationEnvironment = createdEnvironment;
	}
	
	/**
	 * Gets shallow copy of implementations set as Functions
	 * @return Set
	 */
	public Map<Function, Expression> getImplementationsAsFunctions(){
		Map<Function, Expression> m = new TreeMap<Function, Expression>();
		
		for(Entry<? extends Lambda, Expression> e : this.implementations.entrySet()) {
			m.put((Function)e.getKey(), e.getValue());
		}
		
		return m;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		try {
			Set<Pair<Type, Substitution>> s = new HashSet<Pair<Type, Substitution>>();
			for (Expression e : this.implementations.keySet()) {
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
			return this.creationEnvironment.equals(((ExtendedFunction) other).creationEnvironment)
					&& super.equals(other);

		}
		return false;
	}

	@Override
	public String toString() {
		StringBuilder s = new StringBuilder("(ExFunctionInternal (");

		// Arguments
		Iterator<? extends Lambda> k = this.implementations.keySet().iterator();
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
			Environment createdEnvironment, TypeEnvironment typeEnv) throws AppendableException {
		return ExtendedFunction.makeExtendedFunction(implementations, createdEnvironment,
				ExtendedLambda.defaultSelectionFunction, typeEnv);
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
			Environment createdEnvironment, Expression rankingFunction, TypeEnvironment typeEnv) 
					throws AppendableException {
		Type.unifyMany(implementations.stream().map(x -> x.argsType).collect(Collectors.toSet()));
		
		Map<Function, Expression> m = new TreeMap<Function, Expression>();
		for(Function f : implementations) {
			Expression e = f.defaultCostFunction().interpret(createdEnvironment, typeEnv);
			m.put(f, e);
		}
		
		return new ExtendedFunction(m, rankingFunction, createdEnvironment);
	}
	
	/**
	 * 
	 * @param implementations
	 * @param createdEnvironment
	 * @return
	 * @throws AppendableException
	 */
	public static ExtendedFunction makeExtendedFunction(
			Map<Function, Expression> implementations, 
			Environment createdEnvironment) throws AppendableException {
		Type.unifyMany(implementations.keySet().stream().map(x -> x.argsType).collect(Collectors.toSet()));
		
		return new ExtendedFunction(implementations, createdEnvironment);
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
