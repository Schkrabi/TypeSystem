package types;

import util.AppendableException;

/**
 * Exception for unrecognized type names
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class TypeNotRecognizedException extends AppendableException {

	/**
	 * 
	 */
	private static final long serialVersionUID = -1456218599071164301L;

	/**
	 * Name of unrecognized type
	 */
	public final String typeName;

	public TypeNotRecognizedException(String type) {
		super("Type " + type + " was not recognized");
		this.typeName = type;
	}
}
