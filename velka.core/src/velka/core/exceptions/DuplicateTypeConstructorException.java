package velka.core.exceptions;

import velka.core.abstraction.Abstraction;
import velka.types.Type;
import velka.util.AppendableException;

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
	public final Abstraction originalConstructor;

	/**
	 * Newly defined constructor
	 */
	public final Abstraction newConstructor;

	public DuplicateTypeConstructorException(Type conflictedType, Abstraction originalConstructor,
			Abstraction newConstructor) {
		super("Trying to define duplicate constructor for type " + conflictedType + " original constructor "
				+ originalConstructor + " new constructor " + newConstructor);
		this.originalConstructor = originalConstructor;
		this.newConstructor = newConstructor;
		this.conflictedType = conflictedType;
	}
}
