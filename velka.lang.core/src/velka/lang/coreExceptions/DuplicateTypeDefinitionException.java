package velka.lang.coreExceptions;

import velka.lang.types.TypeAtom;
import velka.lang.util.AppendableException;

/**
 * Thrown when TypeAtom is defined multiple times in TypeEnvironment
 * @author Mgr. Radomir Skrabal
 *
 */
public class DuplicateTypeDefinitionException extends AppendableException {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1374142970759527565L;
	/**
	 * Duplicated type
	 */
	public final TypeAtom type;
	
	public DuplicateTypeDefinitionException(TypeAtom type) {
		super("Type " + type.toString() + " was already defined!");
		this.type = type;
	}
}
