package velka.core.util;

import velka.core.interpretation.Environment;
import velka.util.AppendableException;

/**
 * Interface for classes declarable in TypeEnvironment
 * @author Mgr. Radomir �krabal
 *
 */
public interface DeclarableInTypeEnvironment {
	/**
	 * Declares this object in type environment
	 * @param env environment associated with type environment
	 * @param typeEnv type enviroment where declaring
	 * @throws AppendableException if anything goes awry
	 */
	public void declareInTypeEnvironment(Environment env) throws AppendableException;
}
