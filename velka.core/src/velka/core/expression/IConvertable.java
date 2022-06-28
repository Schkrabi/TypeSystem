package velka.core.expression;

import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.types.Type;
import velka.util.AppendableException;

/**
 * Interface for expressions that can be converted.
 * @author Mgr. Radomir Skrabal
 *
 */
public interface IConvertable<T> {

	/**
	 * Converts this to given type.
	 * @param to Type to be converted to
	 * @return Converted expression
	 * @throws AppendableException if conversion is not possible
	 */
	public abstract T convert(Type to, Environment env, TypeEnvironment typeEnv) throws AppendableException;
}
