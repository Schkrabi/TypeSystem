package velka.types;

import velka.util.AppendableException;

/**
 * Class for exception thrown when unexpected type is encountered
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class UnexpectedTypeException extends AppendableException {

	/**
	 * 
	 */
	private static final long serialVersionUID = 377762739016677881L;
	/**
	 * Encountered type
	 */
	public final Type received;
	/**
	 * Expected type class
	 */
	public final Class<? extends Type> expected;

	public UnexpectedTypeException(Type received, Class<? extends Type> expected) {
		super("Got unexpected type " + received.toString() + " expected " + expected.getName());
		this.received = received;
		this.expected = expected;
	}
}
