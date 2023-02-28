package velka.core.exceptions;

import velka.util.AppendableException;

/**
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class FallThroughException extends AppendableException {
	/**
	 * 
	 */
	private static final long serialVersionUID = -1331688638030342518L;
	
	/**
	 * Inner exception
	 */
	public final Exception cause;
	
	public FallThroughException(Exception cause) {
		this.cause = cause;
	}
	
	@Override
	public String toString() {
		return this.cause.toString();
	}
}
