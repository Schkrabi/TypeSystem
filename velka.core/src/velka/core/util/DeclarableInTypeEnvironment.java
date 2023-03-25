package velka.core.util;

import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.util.AppendableException;

/**
 * Interface for classes declarable in TypeEnvironment
 * @author Mgr. Radomir Škrabal
 *
 */
public interface DeclarableInTypeEnvironment {
	/**
	 * Declares this object in type environment
	 * @param env environment associated with type environment
	 * @param typeEnv type enviroment where declaring
	 * @throws AppendableException if anything goes awry
	 */
	public void declareInTypeEnvironment(Environment env, TypeEnvironment typeEnv) throws AppendableException;
}
