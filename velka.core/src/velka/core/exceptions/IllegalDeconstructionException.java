package velka.core.exceptions;

import velka.core.expression.Expression;
import velka.types.Type;
import velka.util.AppendableException;

/**
 * Exception caused by deconstruction to illegal types
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class IllegalDeconstructionException extends AppendableException {
	/**
	 * 
	 */
	private static final long serialVersionUID = 175437823376377815L;
	/**
	 * Deconstructed expression
	 */
	public final Expression deconstructed;
	/**
	 * Type expression should be deconstructed to
	 */
	public final Type deconstructedAs;
	
	public IllegalDeconstructionException(Expression deconstructed, Type deconstructedAs) {
		super("Error deconstructing expression " + deconstructed + " as " + deconstructedAs);
		this.deconstructed = deconstructed;
		this.deconstructedAs = deconstructedAs;
	}
	
}
