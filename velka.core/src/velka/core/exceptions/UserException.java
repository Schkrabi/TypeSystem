package velka.core.exceptions;

import velka.util.AppendableException;

/**
 * Exception class for user defined language exceptions
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class UserException extends AppendableException {

	/**
	 * 
	 */
	private static final long serialVersionUID = 2538053084132717062L;

	public UserException(String msg) {
		super("User exception: " + msg);
	}
}
