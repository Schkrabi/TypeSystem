package expression;

import interpretation.Environment;
import semantic.UserException;
import types.Substitution;
import types.Type;
import util.AppendableException;
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
	public Expression interpret(Environment env) throws Exception {
		throw new UserException(this.message.interpret(env).toString());
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		try {
			//TODO implement
			throw new AppendableException("Not implemented!");
		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	@Override
	public String toClojureCode() throws Exception {
		return "(throw (Throwable. \"" + this.message + "\"))";
	}
	
	@Override
	public String toString() {
		return "error " + this.message.toString();
	}
	
	@Override
	public int compareTo(Expression other) {
		if(other instanceof ExceptionExpr) {
			return this.message.compareTo(((ExceptionExpr) other).message);
		}
		return super.compareTo(other);
	}

}
