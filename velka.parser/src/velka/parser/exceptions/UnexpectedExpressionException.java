package velka.parser.exceptions;

import velka.util.AppendableException;

/**
 * Exception for stubling upon unexpected token type
 * 
 * @author r.SKRABAL
 *
 */
public class UnexpectedExpressionException extends AppendableException {
	/**
	 * 
	 */
	private static final long serialVersionUID = -598536964300191758L;

	/**
	 * Unexpected token
	 */
	public final String unexpected;

	public UnexpectedExpressionException(String unexpected) {
		super("Unexpected expression " + unexpected);
		this.unexpected = unexpected.toString();
	}
}
