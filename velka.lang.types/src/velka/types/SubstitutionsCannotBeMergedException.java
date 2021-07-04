package velka.types;

import velka.types.Substitution;
import velka.util.AppendableException;

/**
 * Exception thrown if two substitutions cannot be merged
 * @author Mgr. Radomir Skrabal
 *
 */
public class SubstitutionsCannotBeMergedException extends AppendableException {

	/**
	 * Serial version Id
	 */
	private static final long serialVersionUID = -7159387631997733262L;
	
	/**
	 * First merged substitution
	 */
	public final Substitution first;
	
	/**
	 * Second merged substitution
	 */
	public final Substitution second;
	
	public SubstitutionsCannotBeMergedException(Substitution first, Substitution second) {
		super("Cannot union substitution " + first.toString() + " with " + second.toString());
		this.first = first;
		this.second = second;
	}
}
