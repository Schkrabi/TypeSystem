package expression;

import interpretation.Environment;
import semantic.UserException;
import types.Type;
import types.TypeConcrete;
import types.TypeVariable;
import util.NameGenerator;

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
	public Type infer(Environment env) throws Exception {
		if(this.getType() == null)
		{
			//Creates a new mock type to unify with anything
			this.setType(new TypeVariable(NameGenerator.next()));
		}
		
		Type argType = this.message.infer(env);
		if(!Type.unify(TypeConcrete.TypeString, argType))
		{
			throw new Exception("Type of message " + message.toString() + " (" + argType.toString() + ") does not unify with " + TypeConcrete.TypeString.toString());
		}
		
		return this.getType();
	}

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) throws Exception {
		//No variables
		return this;
	}

	@Override
	public String toClojureCode() throws Exception {
		return "(throw (Throwable. \"" + this.message + "\"))";
	}
	
	@Override
	public String toString() {
		return "error " + this.message.toString();
	}

}
