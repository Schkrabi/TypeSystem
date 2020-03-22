package abstraction;

import java.util.Iterator;

import expression.Expression;
import expression.Symbol;
import expression.Tuple;
import interpretation.Environment;
import types.Type;
import types.TypeTuple;
import util.AppendableException;

/**
 * Parent class for all abstractions (lambdas, extended lambdas, functions,
 * extended functions and operators)
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public abstract class Abstraction extends Expression {

	protected abstract Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException;
	
	public Expression substituteAndEvaluate(Tuple args, Environment env) throws AppendableException{
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

			ret.put(name, j.next().convertTo(arg, k.next()));
		}

		return ret;
	}

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
	
	
}
