package velka.lang.application;

import java.util.Arrays;

import velka.lang.conversions.Conversions;
import velka.lang.expression.Expression;
import velka.lang.expression.Tuple;
import velka.lang.interpretation.Environment;
import velka.lang.exceptions.UserException;
import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeAtom;
import velka.lang.types.TypeTuple;
import velka.lang.types.TypeVariable;
import velka.lang.util.AppendableException;
import velka.lang.util.NameGenerator;
import velka.lang.util.Pair;

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
			Substitution s = Type.unifyTypes(infered.first, TypeAtom.TypeStringNative);
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
	
	@Override
	protected Tuple convertArgs(Tuple args, Environment env) throws AppendableException {
		Expression msg = args.get(0);
		Pair<Type, Substitution> p = msg.infer(env);
		Tuple t = new Tuple(Arrays.asList(Conversions.convert(p.first, msg, TypeAtom.TypeStringNative)));
		return t;
	}
}
