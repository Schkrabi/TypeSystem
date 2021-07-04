package velka.lang.coreExceptions;

import velka.lang.expression.Symbol;
import velka.lang.util.AppendableException;

/**
 * Exception for evaluating unbound variables
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class UnboundVariableException extends AppendableException {

	/**
	 * 
	 */
	private static final long serialVersionUID = 8468174170443586209L;
	/**
	 * Variable that caused the exception
	 */
	public final Symbol unboundVariable;

	public UnboundVariableException(Symbol unboundVariable) {
		super("Variable " + unboundVariable.toString() + " has no binding.");
		this.unboundVariable = unboundVariable;
	}
}
