package velka.lang.coreExceptions;

import velka.lang.expression.Expression;
import velka.lang.types.Type;
import velka.lang.util.AppendableException;

/**
 * Exception for invalid type conversion occurence
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class ConversionException extends AppendableException {
	/**
	 * 
	 */
	private static final long serialVersionUID = -695682878010968390L;
	/**
	 * Type from which conversion was attempted
	 */
	public final Type fromType;
	/**
	 * Type to which conversion was attempted
	 */
	public final Type toType;
	/**
	 * Expression which should have been converted
	 */
	public final Expression converted;

	public ConversionException(Type fromType, Type toType, Expression converted) {
		super("Trying to convert uncovertable types " + fromType.toString() + " to " + toType.toString());
		this.fromType = fromType;
		this.toType = toType;
		this.converted = converted;
	}
}
