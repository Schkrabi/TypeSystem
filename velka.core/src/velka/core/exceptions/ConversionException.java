package velka.core.exceptions;

import velka.core.expression.Expression;
import velka.types.Type;
import velka.util.AppendableException;

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
	 * Type to which conversion was attempted
	 */
	public final Type to;
	/**
	 * Expression which should have been converted
	 */
	public final Expression converted;

	public ConversionException(Type to, Expression converted) {
		super("Cannot convert " + converted.toString() + " to " + to.toString());
		this.to = to;
		this.converted = converted;
	}
}
