package velka.lang.coreExceptions;

import velka.lang.types.TypeAtom;
import velka.lang.types.TypeTuple;
import velka.lang.util.AppendableException;

/**
 * Exception for requesting unrecognized constructor
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class UnrecognizedConstructorException extends AppendableException {
	/**
	 * 
	 */
	private static final long serialVersionUID = 5972609696982057942L;
	public final TypeAtom constructedType;
	public final TypeTuple requestedArgsType;

	public UnrecognizedConstructorException(TypeAtom constructedType, TypeTuple requestedArgsType) {
		super("Unrecognized constructor used; Construted type was " + constructedType + " with argument types of " + requestedArgsType);
		this.requestedArgsType = requestedArgsType;
		this.constructedType = constructedType;
	}
}
