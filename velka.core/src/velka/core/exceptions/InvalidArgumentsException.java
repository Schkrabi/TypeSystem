package velka.core.exceptions;

import velka.core.abstraction.Abstraction;
import velka.core.expression.Tuple;
import velka.util.AppendableException;

/**
 * Thrown when inference detects wrong types at application
 * @author Mgr. Radomir Skrabal
 *
 */
public class InvalidArgumentsException extends AppendableException {
	/**
	 * 
	 */
	private static final long serialVersionUID = -7151070137289433850L;
	/**
	 * Applied abstraction
	 */
	public final Abstraction abst;
	/**
	 * Supplied arguments
	 */
	public final Tuple suppliedArgs;
	
	public InvalidArgumentsException(Abstraction abst, Tuple suppliedArgs) {
		super("Abstraction " + abst.toString() + " cannot be applied with arguments " + suppliedArgs.toString());
		this.abst = abst;
		this.suppliedArgs = suppliedArgs;
	}
}
