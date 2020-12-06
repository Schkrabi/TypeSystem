package velka.lang.exceptions;

import velka.lang.parser.SemanticNode;
import velka.lang.util.AppendableException;

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
	public final SemanticNode unexpected;

	public UnexpectedExpressionException(SemanticNode unexpected) {
		super("Unexpected expression " + unexpected);
		this.unexpected = unexpected;
	}
}
