/**
 * 
 */
package util;

import java.util.function.Function;

/**
 * @author r.skrabal
 *
 */
@FunctionalInterface
public interface ThrowingFunction<T, R> extends Function<T, R> {
	@Override
	default R apply(final T o) {
		try {
            return applyThrows(o);
        } catch (final AppendableException e) {
            throw new RuntimeException(e);
        }
	}

	R applyThrows(T o) throws AppendableException;
}
