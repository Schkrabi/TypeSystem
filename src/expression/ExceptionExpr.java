package expression;

import interpretation.Environment;
import semantic.UserException;
import types.Substitution;
import types.Type;
import types.TypeVariable;
import util.AppendableException;
import util.NameGenerator;
import util.Pair;

/**
 * Expression for user defined exception
 * @author Mgr. Radomir Skrabal
 *
 */
public class ExceptionExpr extends Expression {
	
	/**
	 * Message of the exception
	 */
	public final Expression message;
	
	public ExceptionExpr(Expression message) {
		this.message = message;
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		throw new UserException(this.message.interpret(env).toString());
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		try {
			Pair<Type, Substitution> infered = this.message.infer(env);
			return new Pair<Type, Substitution>(new TypeVariable(NameGenerator.next()), infered.second);
		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	@Override
	public String toClojureCode() {
		return this.toClojureCode(null, Environment.topLevelEnvironment);
	}
	
	@Override
	protected String toClojureCode(Type expectedType, Environment env) {
		return "(throw (Throwable. " + this.message + "))";
	}
	
	@Override
	public String toString() {
		return "(error " + this.message.toString() + ")";
	}
	
	@Override
	public int compareTo(Expression other) {
		if(other instanceof ExceptionExpr) {
			return this.message.compareTo(((ExceptionExpr) other).message);
		}
		return super.compareTo(other);
	}

	@Override
	public boolean equals(Object other) {
		if(other instanceof ExceptionExpr) {
			return this.message.equals(((ExceptionExpr) other).message);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return this.message.hashCode();
	}
}
