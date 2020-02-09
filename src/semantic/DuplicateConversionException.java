package semantic;

import expression.Expression;
import types.TypeAtom;
import util.AppendableException;

/**
 * Exception thrown when duplicate conversion
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class DuplicateConversionException extends AppendableException {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1271294611059515340L;

	/**
	 * Type from which is conversion defined
	 */
	public final TypeAtom fromType;
	/**
	 * Type to which is converted
	 */
	public final TypeAtom toType;
	/**
	 * Previously defined conversion function
	 */
	public final Expression originalConstructor;
	/**
	 * Newly defined conversion function
	 */
	public final Expression newConstructor;

	public DuplicateConversionException(TypeAtom fromType, TypeAtom toType, Expression originalConstructor,
			Expression newConstructor) {
		super("Trying to define duplicate conversion from type " + fromType + " to type " + toType
				+ ", original conversion " + originalConstructor + " new conversion " + newConstructor);
		this.fromType = fromType;
		this.toType = toType;
		this.originalConstructor = originalConstructor;
		this.newConstructor = newConstructor;
	}
}
