package velka.core.exceptions;

import velka.util.AppendableException;

/**
 * Exception class for reporting uknown type
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class UndefinedTypeException extends AppendableException {
	/**
	 * 
	 */
	private static final long serialVersionUID = 8444385088275160995L;
	public final String unknowTypeName;

	public UndefinedTypeException(String unknownTypeName) {
		super("Unknown type " + unknownTypeName);
		this.unknowTypeName = unknownTypeName;
	}
}
