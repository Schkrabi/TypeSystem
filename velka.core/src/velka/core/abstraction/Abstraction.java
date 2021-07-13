package velka.core.abstraction;

import java.util.Iterator;
import java.util.Optional;

import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.ClojureHelper;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.types.Substitution;
import velka.types.Type;
import velka.util.AppendableException;
import velka.util.Pair;

/**
 * Parent class for all abstractions (lambdas, extended lambdas, functions,
 * extended functions and operators)
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public abstract class Abstraction extends Expression {

	protected abstract Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv, Optional<Expression> rankingFunction) throws AppendableException;

	public Expression substituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv, Optional<Expression> rankingFunction) throws AppendableException {
		return this.doSubstituteAndEvaluate(args, env, typeEnv, rankingFunction);
	}

	/**
	 * Creates lexical clojure for this abstraction.
	 * Runtime interpretation method
	 * 
	 * @param formalArgs      formal arguments (argument variables)
	 * @param realArgs        arguments
	 * @param baseEnvironment environment where body of abstraction will be
	 *                        evaluated, also parent environment of lexical clojure
	 * @return Environment that is lexical clojure with given formal and real
	 *         arguments
	 * @throws AppendableException
	 */
	protected static Environment lexicalClojure(Tuple formalArgs, Tuple realArgs, Environment baseEnvironment)
			throws AppendableException {
		Environment ret = Environment.create(baseEnvironment);
		Iterator<Expression> i = formalArgs.iterator();
		Iterator<Expression> j = realArgs.iterator();

		while (i.hasNext()) {
			Expression e = j.next();
			Symbol v = (Symbol) i.next();
			ret.put(v, e);
		}

		return ret;
	}
	
	/**
	 * Creates clojure code for implementations of this abstraction
	 * @param env environemnt where evaluation takes place
	 * @return String containing clojure code
	 * @throws AppendableException
	 */
	protected abstract String implementationsToClojure(Environment env, TypeEnvironment typeEnv) throws AppendableException;
	
	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException{
		Pair<Type, Substitution> p = this.infer(env, typeEnv);
		String code = ClojureHelper.addTypeMetaInfo(this.implementationsToClojure(env, typeEnv), p.first);
		
		return code;
	}
	
	/**
	 * Selects implementation for this abstraction based on arguments and ranking function
	 * Runtime interpretation method
	 * 
	 * @param args arguments of application
	 * @param rankingFunction ranking function
	 * @return abstraction
	 * @throws AppendableException 
	 */
	public abstract Abstraction selectImplementation(Tuple args, Optional<Expression> rankingFunction, Environment env,
			TypeEnvironment typeEnv) throws AppendableException;
}