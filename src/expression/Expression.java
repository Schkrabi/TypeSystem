package expression;

import types.Type;
import interpretation.Environment;

/**
 * Abstract superclass for all expressions
 * 
 * @author schkrabi
 * 
 */
public abstract class Expression {
	/**
	 * Type infered by the infer method when it is run
	 */
	private Type inferedType = null;

	/**
	 * Interprets the expression in given environment
	 * 
	 * @param env
	 *            environment where the expression should be intepreted
	 * @return Expression
	 * @throws Exception
	 */
	public abstract Expression interpret(Environment env) throws Exception;

	/**
	 * Ifers the type of the expression and all its subexpression and returns
	 * it. Also sets the inferedType variable of each infered expression
	 * 
	 * @return infered type
	 * @throws Exception
	 */
	public abstract Type infer() throws Exception;

	/**
	 * Returns the ifered type of this expression or null if inference was not
	 * run
	 * 
	 * @return Type or null
	 */
	public Type getType() {
		return this.inferedType;
	}

	/**
	 * Sets the type of the expression
	 * 
	 * @warning Disougradge of use outsiode of the infer methos!
	 * @param value
	 *            type to be set
	 */
	protected void setType(Type value) {
		this.inferedType = value;
	}
}
