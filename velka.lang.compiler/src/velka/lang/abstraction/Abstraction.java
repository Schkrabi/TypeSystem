package velka.lang.abstraction;

import java.util.Iterator;

import velka.lang.conversions.Conversions;
import velka.lang.expression.Expression;
import velka.lang.expression.Symbol;
import velka.lang.expression.Tuple;
import velka.lang.interpretation.Environment;
import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeTuple;
import velka.lang.util.AppendableException;
import velka.lang.util.Pair;

/**
 * Parent class for all abstractions (lambdas, extended lambdas, functions,
 * extended functions and operators)
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public abstract class Abstraction extends Expression {

	protected abstract Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException;

	public Expression substituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
		return this.doSubstituteAndEvaluate(args, env);
	}

	/**
	 * Lazily converts all the arguments to given representation
	 * 
	 * @param e        environment containing the arguments associated with their
	 *                 names
	 * @param args     argument names in the environment
	 * @param argTypes formal inferred types of the arguments
	 * @return new environment where all the arguments will be converted
	 * @throws Exception
	 */
	protected static Environment autoConvertArgs(Environment e, Tuple args, TypeTuple fromType, TypeTuple toType)
			throws AppendableException {
		Environment ret = Environment.create(e.parent);

		Iterator<Expression> i = args.iterator();
		Iterator<Type> j = fromType.iterator();
		Iterator<Type> k = toType.iterator();
		while (i.hasNext()) {
			Symbol name = (Symbol) i.next();
			Expression arg = e.getVariableValue(name);

			ret.put(name, Conversions.convert(j.next(), arg, k.next()));
		}

		return ret;
	}

	/**
	 * Creates lexical clojure for this abstraction
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
	protected abstract String implementationsToClojure(Environment env) throws AppendableException;
	
	@Override
	public String toClojureCode(Environment env) throws AppendableException{
		StringBuilder sb = new StringBuilder();
		sb.append("(with-meta [");
		sb.append(this.implementationsToClojure(env));
		sb.append("] {:lang-type ");
		
		Pair<Type, Substitution> p = this.infer(env);
		sb.append(p.first.clojureTypeRepresentation());
		sb.append("})");
		return sb.toString();
	}

}
