package velka.core.exceptions;

import velka.core.abstraction.Abstraction;
import velka.core.expression.Expression;
import velka.core.expression.Tuple;
import velka.util.AppendableException;

/**
 * Thrown if selection function is badly formed
 * @author Mgr. Radomir Skrabal
 *
 */
public class MalformedSelectionFunctionException extends AppendableException {

	/**
	 * Serial version id
	 */
	private static final long serialVersionUID = -2311245139666803887L;
	
	/**
	 * Exception occurred during application of this abstraction
	 */
	public final Abstraction appliedAbstraction;
	/**
	 * Exception occurred for this selection function
	 */
	public final Expression selectionFunction;
	/**
	 * args passed to the application (both to appliedAbstraction and selectionFunction)
	 */
	public final Tuple args;
	/**
	 * Expression returned by selection function
	 */
	public final Expression returnedExpression;
	
	public MalformedSelectionFunctionException(
			Abstraction appliedAbstraction,
			Expression selectionFunction,
			Tuple args,
			Expression returnedExpression) {
		super("Error: Malformed selection function " + selectionFunction.toString() + " returned "
				+ returnedExpression.toString() + " for abstraction " + appliedAbstraction.toString()
				+ " with arguments " + args.toString());
		this.appliedAbstraction = appliedAbstraction;
		this.selectionFunction = selectionFunction;
		this.args = args;
		this.returnedExpression = returnedExpression;
	}
	
}
