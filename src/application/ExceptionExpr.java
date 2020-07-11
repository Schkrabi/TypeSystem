package application;

import java.util.Arrays;

import expression.Expression;
import expression.Tuple;
import interpretation.Environment;
import semantic.UserException;
import types.Substitution;
import types.Type;
import types.TypeAtom;
import types.TypeTuple;
import types.TypeVariable;
import util.AppendableException;
import util.NameGenerator;
import util.Pair;

/**
 * Expression for user defined exception
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class ExceptionExpr extends SpecialFormApplication {

	public ExceptionExpr(Expression message) {
		super(new Tuple(Arrays.asList(message)));
	}
	
	public Expression getMessage() {
		return this.args.get(0);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		try {
			Pair<Type, Substitution> infered = this.getMessage().infer(env);
			Substitution s = Type.unify(infered.first, TypeAtom.TypeStringNative);
			s = s.union(infered.second);
			return new Pair<Type, Substitution>(new TypeVariable(NameGenerator.next()), s);
		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof ExceptionExpr) {
			return this.args.equals(((ExceptionExpr) other).args);
		}
		return false;
	}

	@Override
	protected String applicationToClojure(Tuple convertedArgs, Environment env) throws AppendableException {
		return "(throw (Throwable. (get " + convertedArgs.get(0).toClojureCode(env) + " 0)))";
	}

	@Override
	protected String applicatedToString() {
		return "error";
	}

	@Override
	protected Expression apply(Tuple convertedArgs, Environment evaluationEnvironment) throws AppendableException {
		throw new UserException(convertedArgs.get(0).interpret(evaluationEnvironment).toString());
	}

	@Override
	protected TypeTuple getFunArgsType(TypeTuple argsType, Environment env) throws AppendableException {
		return new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative));
	}
}
