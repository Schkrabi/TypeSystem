package semantic;

import expression.Function;
import types.Type;
import util.AppendableException;

/**
 * Exception risen when duplicate constructor definition appears
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class DuplicateTypeConstructorException extends AppendableException {

	/**
	 * 
	 */
	private static final long serialVersionUID = 7329443659464172312L;

	/**
	 * Type over which conflict occured
	 */
	public final Type conflictedType;

	/**
	 * Previously defined constructor of type
	 */
	public final Function originalConstructor;

	/**
	 * Newly defined constructor
	 */
	public final Function newConstructor;

	public DuplicateTypeConstructorException(Type conflictedType, Function originalConstructor,
			Function newConstructor) {
		super("Trying to define duplicate constructor for type " + conflictedType + " original constructor "
				+ originalConstructor + " new constructor " + newConstructor);
		this.originalConstructor = originalConstructor;
		this.newConstructor = newConstructor;
		this.conflictedType = conflictedType;
	}
}
